export class CobolCompiler {
  static async compile(code) {
    // Simulate compilation time
    await new Promise(resolve => setTimeout(resolve, 1000));
    
    const lines = code.split('\n');
    let output = '';
    let hasErrors = false;
    
    // Check for basic COBOL structure
    const hasIdentification = lines.some(line => 
      line.includes('IDENTIFICATION DIVISION')
    );
    const hasProcedure = lines.some(line => 
      line.includes('PROCEDURE DIVISION')
    );
    
    if (!hasIdentification) {
      return { 
        success: false, 
        output: 'ERROR: Missing IDENTIFICATION DIVISION' 
      };
    }
    
    if (!hasProcedure) {
      return { 
        success: false, 
        output: 'ERROR: Missing PROCEDURE DIVISION' 
      };
    }
    
    // Parse DISPLAY statements
    output = this.parseDisplayStatements(lines);
    
    // Parse COMPUTE statements
    output += this.parseComputeStatements(lines);
    
    if (!output) {
      output = 'Program compiled successfully. No output generated.';
    }
    
    return { success: true, output: output.trim() };
  }
  
  static parseDisplayStatements(lines) {
    let output = '';
    const displayLines = lines.filter(line => 
      line.trim().toUpperCase().startsWith('DISPLAY')
    );
    
    displayLines.forEach(line => {
      const match = line.match(/DISPLAY\s+([^.]+)/i);
      if (match) {
        let displayValue = match[1].trim();
        
        if (displayValue.startsWith("'") && displayValue.endsWith("'")) {
          output += displayValue.slice(1, -1) + '\n';
        } else if (displayValue.includes('WS-MESSAGE')) {
          output += 'Hello, COBOL World!\n';
        } else if (displayValue.includes('WS-')) {
          output += `[Variable: ${displayValue}]\n`;
        } else {
          output += displayValue + '\n';
        }
      }
    });
    
    return output;
  }
  
  static parseComputeStatements(lines) {
    let output = '';
    const computeLines = lines.filter(line => 
      line.trim().toUpperCase().includes('COMPUTE')
    );
    
    computeLines.forEach(line => {
      const match = line.match(/COMPUTE\s+(\w+)\s*=\s*([^.]+)/i);
      if (match) {
        output += `Computed ${match[1]} = ${match[2]}\n`;
      }
    });
    
    return output;
  }
}