// src/index.ts
import * as ts from 'typescript';
import { parseCONCode } from './CONParser'; // or wherever your parser lives

/**
 * Plugin initialization function:
 */
function init({ typescript: tsModule }: { typescript: typeof ts }) {
    /**
     * Our custom create function: we return a new LanguageService
     * that wraps TypeScript's existing one, injecting custom checks.
     */
    function create(info: ts.server.PluginCreateInfo): ts.LanguageService {
        // 1. Create a new LanguageService that proxies the original
        const baseLS = info.languageService;

        // C) Make a proxy that *starts* as a copy of `baseLS`, not `oldLS`
        const proxy = { ...baseLS };

        function resolveImportedFile(
            program: ts.Program,
            importingFile: ts.SourceFile,
            importPath: string
        ): ts.SourceFile | undefined {
            const compilerOptions = program.getCompilerOptions();

            // 1) Use the official resolution function
            const resolved = ts.resolveModuleName(
                importPath,
                importingFile.fileName,
                compilerOptions,
                ts.sys
            );

            // 2) Check if it found a resolved module
            const resolvedFileName = resolved.resolvedModule?.resolvedFileName;
            if (!resolvedFileName) {
                return undefined;
            }

            // 3) Get the SourceFile from the program
            return program.getSourceFile(resolvedFileName);
        }


        function importsMarkerFileHasAtTypecon(
            program: ts.Program,
            sf: ts.SourceFile
        ): boolean {
            for (const stmt of sf.statements) {
                if (!ts.isImportDeclaration(stmt)) continue;
                if (!ts.isStringLiteral(stmt.moduleSpecifier)) continue;

                // 1) Check if this is the relevant path (you can do exact match or .includes)
                const importPath = stmt.moduleSpecifier.text;
                /*if (!importPath.includes("defs/TCSet100/types")) {
                    continue;
                }*/

                // 2) Resolve the import to a real file
                const importedFile = resolveImportedFile(program, sf, importPath);
                if (!importedFile) continue;

                // 3) Check if the imported file text has '@typecon'
                if (importedFile.getFullText().includes("@typecon")) {
                    return true; // Found it => Good enough, we can stop searching
                }
            }
            return false;
        }


        // 2. Override getSemanticDiagnostics
        proxy.getSemanticDiagnostics = (fileName: string) => {
            // 1) Use baseLS to get normal TS diagnostics
            const priorDiagnostics = baseLS.getSemanticDiagnostics(fileName);

            // 2) Grab the Program from baseLS (which knows about .tsc)
            const program = baseLS.getProgram();
            if (!program) return priorDiagnostics;

            const sourceFile = program.getSourceFile(fileName);
            if (!sourceFile) return priorDiagnostics;

            // 4) Now do your big AST checks (paste your existing code that was in the old override)
            const checker = program.getTypeChecker();
            const customDiagnostics: ts.Diagnostic[] = [];

            // 3) If it's a .ts file with no @typecon marker, skip
            const fileText = sourceFile.getFullText();

            // If we want to see whether the file imports `../defs/TCSet100/types`
            // and that imported file contains `@typecon`, do:
            if (!importsMarkerFileHasAtTypecon(program, sourceFile)) {
                return [...priorDiagnostics, ...customDiagnostics];
            }

            /**
             * =============== HELPER FUNCTIONS ===============
             */

            // A. Check if a type is named exactly "constant" (assuming type alias).
            //    You might want to refine this if "constant" is declared in a module.
            function isConstantType(type: ts.Type): boolean {
                // If it's an alias, check aliasSymbol.name.
                if (type.aliasSymbol?.name === "constant") {
                    return true;
                }
                // Alternatively, if "constant" is an interface or type:
                if (type.symbol && type.symbol.name === "constant") {
                    return true;
                }
                return false;
            }

            // B. Check if an expression is strictly a numeric literal (no expressions).
            function isStrictNumericLiteral(expr: ts.Expression): boolean {
                return ts.isNumericLiteral(expr);
            }

            // C. Check if a parameter is typed as 'constant'
            function paramIsConstant(paramSymbol: ts.Symbol, callSite: ts.CallExpression): boolean {
                const paramType = checker.getTypeOfSymbolAtLocation(paramSymbol, callSite);
                return isConstantType(paramType);
            }

            function isAllowedConstantArgument(expr: ts.Expression): boolean {
                // 1) If it's a numeric literal like `42`
                if (ts.isNumericLiteral(expr)) {
                    if(expr.text.includes('.'))
                        pushDiagnostic(expr, `Decimal values are not allowed`, ts.DiagnosticCategory.Error);
                    return true;
                }

                // 2) Otherwise, check if the type is an enum or enum literal
                const argType = checker.getTypeAtLocation(expr);

                // If it's a union, we might need to check each constituent. For simplicity:
                // - We'll handle the simplest case: the type is a single enum or enum literal.
                if (isEnumOrEnumLiteral(argType)) {
                    return true;
                }

                // Could extend logic for more advanced scenarios (unions, etc.)
                return false;
            }

            function isEnumOrEnumLiteral(t: ts.Type): boolean {
                // The type flags for enum-related types:
                //   - EnumLiteral = 1<<15
                //   - Enum       = 1<<14
                const ENUM_FLAGS = ts.TypeFlags.EnumLiteral | ts.TypeFlags.Enum;

                // If it's a union, check each part:
                if (t.isUnion()) {
                    return t.types.some(part => (part.flags & ENUM_FLAGS) !== 0);
                }

                // If it’s a non-union type:
                return (t.flags & ENUM_FLAGS) !== 0;
            }

            // D. Produce a diagnostic easily
            function pushDiagnostic(node: ts.Node, message: string, category = ts.DiagnosticCategory.Error) {
                customDiagnostics.push({
                    file: sourceFile,
                    start: node.getStart(),
                    length: node.getWidth(),
                    messageText: message,
                    category,
                    code: 9999 // pick a unique code
                });
            }

            // E. Parse "CON" code with a hypothetical parser
            function validateCONString(arg: ts.Expression) {
                // We only handle string literals
                if (!ts.isStringLiteral(arg) && !ts.isNoSubstitutionTemplateLiteral(arg)) {
                    pushDiagnostic(arg, `CON(...) argument must be a string literal or template literal without placeholders.`);
                    return;
                }
                const text = arg.getText().slice(1, -1);;
                const conErrors = parseCONCode(text);
                if (conErrors.length > 0) {
                    for (const err of conErrors) {
                        pushDiagnostic(arg, `CON code error: ${err.message}`);
                    }
                }
            }

            // F. Check if a class is named or extends 'CActor' or 'CEvent'
            function classIsCActorOrCEvent(node: ts.ClassDeclaration): boolean {
                if (!node.name) return false;
                const nameText = node.name.text;
                if (nameText === "CActor" || nameText === "CEvent") return true;

                // or check extends: e.g. "class Foo extends CActor"
                if (node.heritageClauses) {
                    for (const clause of node.heritageClauses) {
                        for (const type of clause.types) {
                            const t = checker.getTypeAtLocation(type.expression);
                            if (t.symbol?.name === "CActor" || t.symbol?.name === "CEvent") {
                                return true;
                            }
                        }
                    }
                }
                return false;
            }

            function returnClassExtension(node: ts.ClassDeclaration): string | undefined {
                if (!node.name) return undefined;
                const nameText = node.name.text;
                if (nameText === "CActor" || nameText === "CEvent") return nameText;

                // or check extends: e.g. "class Foo extends CActor"
                if (node.heritageClauses) {
                    for (const clause of node.heritageClauses) {
                        for (const type of clause.types) {
                            const t = checker.getTypeAtLocation(type.expression);
                            if (t.symbol?.name === "CActor" || t.symbol?.name === "CEvent") {
                                return t.symbol?.name;
                            }
                        }
                    }
                }

                return undefined;
            }

            // G. Check if a property type is IAction, IMove, or IAi
            function isAllowedActionType(type: ts.Type): boolean {
                const name = type.symbol?.name;
                return name === "IAction" || name === "IMove" || name === "IAi";
            }

            // H. Track "block scope" references. A simplistic approach:
            //    We'll gather variables declared in each block, then forbid
            //    references from inner or outer blocks. (This is quite unusual,
            //    but it shows the concept.)
            //    In a real scenario, you might do a full symbol table approach.
            interface Scope {
                parent: Scope | null;
                declaredVars: ts.Symbol[];
            }
            let currentScope: Scope = { parent: null, declaredVars: [] };

            function enterScope() {
                currentScope = { parent: currentScope, declaredVars: [] };
            }
            function exitScope() {
                if (currentScope.parent) {
                    currentScope = currentScope.parent;
                }
            }

            function declareSymbolInScope(symbol: ts.Symbol) {
                currentScope.declaredVars.push(symbol);
            }

            function isSymbolInThisScope(symbol: ts.Symbol, scope: Scope): boolean {
                return scope.declaredVars.includes(symbol);
            }

            function checkBlockScopeReference(symbol: ts.Symbol, node: ts.Identifier) {
                // We want to disallow referencing variables from *any other* scope.
                // So the symbol must be in the *current* scope only, not in parent or child.
                // We'll do a quick approach: the moment we see a reference,
                // we see if it's declared in the currentScope exactly. If not, error.
                if (!isSymbolInThisScope(symbol, currentScope)) {
                    pushDiagnostic(node, `Illegal block-scoped reference to '${node.text}'.`);
                }
            }

            // I. For the "free memory" check, track variables that need a 'free' call:
            //    We'll store them in a Map<symbolId, { node, freed }>
            //    then if we never see a delete or .Free() call, we error.
            type MemTrackData = { node: ts.VariableDeclaration; freed: boolean };
            const memoryTracker = new Map<ts.Symbol, MemTrackData>();

            function trackAllocation(sym: ts.Symbol, decl: ts.VariableDeclaration) {
                memoryTracker.set(sym, { node: decl, freed: false });
            }
            function markFreed(sym: ts.Symbol) {
                const tracked = memoryTracker.get(sym);
                if (tracked) {
                    tracked.freed = true;
                }
            }

            /**
             * =============== AST TRAVERSAL ===============
             * We'll do a single pass with a recursive visit.
             * In real usage, you might want multiple passes or more careful logic.
             */
            function visit(node: ts.Node) {
                // 1. Check function calls
                if (ts.isCallExpression(node)) {
                    const signature = checker.getResolvedSignature(node);
                    if (signature) {
                        const params = signature.getParameters();
                        node.arguments.forEach((arg, index) => {
                            const paramSym = params[index];
                            if (!paramSym) return;
                            const paramType = checker.getTypeOfSymbolAtLocation(paramSym, node);

                            if(ts.isNumericLiteral(arg)) {
                                if(arg.text.includes('.'))
                                    pushDiagnostic(arg, `Decimal values are not allowed`, ts.DiagnosticCategory.Error);
                            }

                            if(ts.isStringLiteral(arg)) {
                                if(arg.text.length > 128)
                                    pushDiagnostic(arg, `String cannot be longer than 128 chracters`, ts.DiagnosticCategory.Warning);
                            }

                            // A) If param is typed as 'constant', ensure it's a numeric literal
                            if (paramIsConstant(paramSym, node)) {
                                if (!isAllowedConstantArgument(arg)) {
                                    pushDiagnostic(
                                        arg,
                                        `Parameter of type 'constant' only accepts a pure numeric literal (no variables or expressions).`
                                    );
                                }
                            }
                        });

                        // B) If the called function is named "CON"
                        //    We assume a function like:  declare function CON(native_code: string): void;
                        const decl = signature.getDeclaration();
                        if (decl) {
                            const declSym = checker.getSymbolAtLocation(decl.name ?? decl);

                            if (declSym) {
                                if (declSym.name === "CON" || declSym.name == 'CONUnsafe') {
                                    // Expect 1 arg that must be a string literal in "CON" language
                                    if (node.arguments.length !== 1) {
                                        pushDiagnostic(node, `CON(...) requires exactly 1 argument.`);
                                    } else {
                                        validateCONString(node.arguments[0]);
                                    }
                                }
                            }
                        }
                    }
                }

                // 2. Check property accesses: forbid bind/call/apply
                if (ts.isPropertyAccessExpression(node)) {
                    const propName = node.name.text;
                    if (propName === "bind" || propName === "call" || propName === "apply"
                        || propName == 'map' || propName == 'reduce'
                    ) {
                        pushDiagnostic(node, `Usage of .${propName}() is not allowed.`);
                    }
                }

                // 3. If we have a class declaration for CActor/CEvent, enforce restrictions
                if (ts.isClassDeclaration(node) && classIsCActorOrCEvent(node)) {
                    checkCActorCEventRules(node);
                }

                // 4. Keep track of block scopes
                //    Enter a new scope when we see a block/constructor/arrow function, etc.
                if (ts.isBlock(node) || ts.isSourceFile(node)) {
                    enterScope();
                    node.forEachChild(visit);
                    exitScope();
                    return; // we've visited children already
                }

                // 5. Track variable declarations for memory usage
                if (ts.isVariableDeclaration(node)) {
                    if (node.name && ts.isIdentifier(node.name)) {
                        const sym = checker.getSymbolAtLocation(node.name);
                        if (sym) {
                            // Record that we declared this symbol in the current scope
                            declareSymbolInScope(sym);

                            // If it's allocated with new, or array literal [], or Array() with no args
                            const init = node.initializer;
                            if (init) {
                                if(ts.isNumericLiteral(init)) {
                                    if(init.text.includes('.'))
                                        pushDiagnostic(init, `Decimal values are not allowed`, ts.DiagnosticCategory.Error);
                                }
                                if(ts.isStringLiteral(init)) {
                                    if(init.text.length > 128)
                                        pushDiagnostic(init, `String cannot be longer than 128 chracters`, ts.DiagnosticCategory.Warning);
                                }
                                if (ts.isNewExpression(init)) {
                                    // new Something()
                                    trackAllocation(sym, node);
                                } else if (ts.isArrayLiteralExpression(init)) {
                                    // []
                                    trackAllocation(sym, node);
                                } else if (
                                    ts.isCallExpression(init) &&
                                    init.expression.getText() === "Array" &&
                                    init.arguments.length === 0
                                ) {
                                    // Array() with no args
                                    trackAllocation(sym, node);
                                }
                            }
                        }
                    }
                }

                // 6. Check references to variables => enforce block-scope rule + free checks
                if (ts.isIdentifier(node)) {
                    const sym = checker.getSymbolAtLocation(node);
                    if (sym) {
                        // 6A. Enforce block-scope usage
                        //checkBlockScopeReference(sym, node);

                        // 6B. Check if it's 'delete varName;' or 'varName.Free()'
                        //     Actually, 'delete varName;' in TS AST is not standard — that’s usually 'delete obj.prop'
                        //     We'll look for something like `delete varName;` as a unary expression
                        //     or `varName.Free()` as a call expression. We'll do that in the parent node checks.
                    }
                }

                // 7. Check for 'delete varName' or 'varName.Free()'
                //    We'll do this by examining the node's parent or the node itself in detail.
                if (ts.isCallExpression(node)) {
                    if (
                        ts.isIdentifier(node.expression) &&       // the function name is an identifier
                        (node.expression.text === "Free" || node.expression.text === "Delete") &&        // the function name is exactly "free"
                        node.arguments.length === 1               // exactly one argument
                    ) {
                        const arg = node.arguments[0];
                        if (ts.isIdentifier(arg)) {
                            const sym = checker.getSymbolAtLocation(arg);
                            if (sym) {
                                markFreed(sym);
                            }
                        }
                    }
                }

                // Recurse on children
                ts.forEachChild(node, visit);
            }

            // Special function to check the rules for classes that are or extend CActor/CEvent
            function checkCActorCEventRules(cls: ts.ClassDeclaration) {
                // We want to allow:
                // - const properties typed as IAction, IMove, or IAi
                // - a constructor that calls super(...) and does nothing else
                // - NO other variables, functions, or statements

                // (A) For each member:
                let foundMain = false;
                for (const member of cls.members) {
                    // If it's a constructor, let's check its body
                    if (ts.isConstructorDeclaration(member)) {
                        checkCActorCEventConstructor(member);
                    }

                    if(ts.isMethodDeclaration(member)) {
                        if((member.name.getText() == 'Main' && returnClassExtension(cls) == 'CActor')
                        || ((member.name.getText() == 'Append' || member.name.getText() == 'Prepend')
                        && returnClassExtension(cls) == 'CEvent'))
                            foundMain = true;
                    }
                }

                if(!foundMain) {
                    if(returnClassExtension(cls) == 'CActor')
                        pushDiagnostic(cls, `Missing Main method at CActor class`);
                    else pushDiagnostic(cls, `Missing Append or Prepend method at CEvent class`);
                }
            }

            // Check that the constructor has only a super(...) call
            function checkCActorCEventConstructor(ctor: ts.ConstructorDeclaration) {
                if (!ctor.body) return;
                const statements = ctor.body.statements;
                for (const stmt of statements) {
                    // 1) If it's a variable statement, ensure it's 'const' and has the correct type(s)
                    if (ts.isVariableStatement(stmt)) {
                        // Check if the declaration list is flagged as CONST
                        const isConst = (stmt.declarationList.flags & ts.NodeFlags.Const) !== 0;
                        if (!isConst) {
                            pushDiagnostic(
                                stmt,
                                `Only 'const' declarations are allowed here.`
                            );
                        }

                        // Now check each variable declared
                        for (const decl of stmt.declarationList.declarations) {
                            // Must have an explicit type annotation, e.g., "const foo: IAction = ..."
                            if (!decl.type) {
                                pushDiagnostic(
                                    decl,
                                    `Property missing explicit type.`
                                );
                                continue;
                            }

                            // Get the actual ts.Type from the type node
                            const propType = checker.getTypeAtLocation(decl.type);

                            // Check if it's one of IAction, IMove, or IAi
                            if (!isAllowedActionType(propType)) {
                                pushDiagnostic(
                                    decl,
                                    `Only IAction, IMove, or IAi properties are allowed.`
                                );
                            }
                        }

                        continue; // Move on to the next statement
                    }

                    // 2) Otherwise, if it's an expression statement, check if it's 'super(...)'
                    if (ts.isExpressionStatement(stmt)) {
                        const expr = stmt.expression;
                        if (
                            !ts.isCallExpression(expr) ||
                            expr.expression.kind !== ts.SyntaxKind.SuperKeyword
                        ) {
                            pushDiagnostic(stmt, `Only 'super(...)' is allowed in this constructor.`);
                        }
                        continue;
                    }

                    // 3) If you reach here, it means it's some other statement
                    pushDiagnostic(stmt, `No other statements are allowed here.`);
                }

            }

            // =============== Start the traversal ===============
            visit(sourceFile);

            // =============== After the traversal ===============
            // For each tracked variable that was never freed, produce an error
            for (const [symId, trackData] of memoryTracker.entries()) {
                if (!trackData.freed) {
                    const varNameNode = trackData.node.name;
                    pushDiagnostic(
                        varNameNode,
                        `Memory allocated for '${varNameNode.getText()}' was never freed (no 'delete' or 'Free()' call).`
                    );
                }
            }

            // Return the combination of TS diagnostics + our custom diagnostics
            return [...priorDiagnostics, ...customDiagnostics];
        };

        return proxy;
    }

    return { create };
}

export = init;
