# TypeCON Plugin

This is a **TypeScript Language Service Plugin** that enforces custom “TypeCON” rules. It applies its checks **only** to files that import a specific “marker” path (e.g., `defs/TCSet100/types` or other language set). If the marker file itself contains `@typecon`, the importing file is flagged for special analysis.

## Features

- **Marker Import Detection**  
  If a file does:
  ```ts
  import "defs/TCSet100/types";
  ```
  or another recognized pattern, this plugin runs additional checks (e.g., ensuring `CON(...)` usage, restricting block-scope references, etc.).
- **Optional Check for `@typecon`**  
  The plugin can resolve the import path to the actual file on disk, load that file’s source, and confirm `@typecon` is present. If found, the main file is considered a “TypeCON” file.

## How It Works

1. **Patches the TypeScript Language Service**  
   The plugin overrides `getSemanticDiagnostics`, merges in TypeScript’s normal diagnostics, and then adds its own if the file meets your “marker” criteria.
2. **Resolves the Imported File**  
   Using `ts.resolveModuleName` plus `program.getSourceFile(...)`, the plugin finds the real source file behind `'../defs/TCSet100/types'`.
3. **Scans for `@typecon`** (optional)  
   If the imported file’s text includes `@typecon`, we proceed with advanced TypeCON checks. Otherwise, the file is skipped.

## Installation & Usage

1. **Install the plugin** in your TypeScript project:
   ```bash
   npm install --save-dev path/to/this-plugin
   # or
   yarn add --dev path/to/this-plugin
   ```

2. **Add to your `tsconfig.json`**:
   ```jsonc
   {
     "compilerOptions": {
       "plugins": [
         { "name": "typecon-plugin" }
       ]
       // ...
     },
     "include": [
       "src/**/*.ts"
       // ...
     ]
   }
   ```

3. **Open your project** in VS Code (or your editor of choice that uses the TS server).  
4. **Import the marker** in any file you want to apply the rules:
   ```ts
   import "defs/TCSet100/types"; 
   // That file might contain: 
   //   // @typecon
   //   // or other code
   ```

5. **Check** your editor’s Problems panel or run `tsc` to see the plugin’s diagnostics.
6. **Restart TS Server** open the command pallete and run TypeScript: Restart TS Server, so the plugin can start working.

## Example

```ts
// main.ts
import "defs/TCSet100/types"; // triggers TypeCON checks

CON(`
  qputs 9999 Hello
  quote 9999
`); // your plugin can parse & validate this code
```

```ts
// ../defs/TCSet100/types.ts
// @typecon
// (Any code or marker you need here)
```

## Contributing

1. Clone or fork this repository.
2. Install dependencies:
   ```bash
   npm install
   ```
3. Build the plugin:
   ```bash
   npm run build
   ```
4. Test or integrate into a sample TypeScript project to verify the diagnostics appear as expected.

## Troubleshooting

- **Plugin Not Loading**  
  Make sure your main project’s `tsconfig.json` includes:
  ```jsonc
  "compilerOptions": {
    "plugins": [
      { "name": "typecon-plugin" }
    ]
  }
  ```
  and that the plugin is installed in the project’s `node_modules`.

- **Files Not Detected**  
  Ensure your project has `"include": ["src/**/*.ts"]` or a suitable pattern. Also confirm the plugin logic is referencing the correct import path.

- **Crash on Import Resolution**  
  Double-check you’re using the official `ts.resolveModuleName` approach, and your environment can load the marker file. If the marker file is not in your compilation, the plugin may skip or fail to resolve.

- **No Diagnostics**  
  Verify the file is actually importing the marker path, and the marker file has `@typecon` if that logic is required.

---