
export interface CONDiagnostic {
    message: string;
    line?: number;
    column?: number;
  }
  
  // A dummy parser that checks if the string includes "ERROR"
  export function parseCONCode(code: string): CONDiagnostic[] {
    const diagnostics: CONDiagnostic[] = [];
    if (code.includes("ERROR")) {
      diagnostics.push({
        message: "Found 'ERROR' keyword in CON code (example check)."
      });
    }
    return diagnostics;
  }
  