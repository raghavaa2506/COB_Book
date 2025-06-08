export class CobolParser {
  static parseProgram(code) {
    const lines = code.split('\n').map(line => line.trim());
    const program = {
      identification: null,
      environment: null,
      data: {
        workingStorage: [],
        fileSection: []
      },
      procedure: []
    };

    let currentSection = null;
    let currentDivision = null;

    lines.forEach(line => {
      const upperLine = line.toUpperCase();
      
      if (upperLine.includes('IDENTIFICATION DIVISION')) {
        currentDivision = 'identification';
      } else if (upperLine.includes('ENVIRONMENT DIVISION')) {
        currentDivision = 'environment';
      } else if (upperLine.includes('DATA DIVISION')) {
        currentDivision = 'data';
      } else if (upperLine.includes('PROCEDURE DIVISION')) {
        currentDivision = 'procedure';
      } else if (upperLine.includes('WORKING-STORAGE SECTION')) {
        currentSection = 'workingStorage';
      } else if (upperLine.includes('FILE SECTION')) {
        currentSection = 'fileSection';
      }

      // Parse program-id
      if (upperLine.includes('PROGRAM-ID') && currentDivision === 'identification') {
        const match = line.match(/PROGRAM-ID\.\s*([A-Za-z0-9-]+)/i);
        if (match) {
          program.identification = match[1];
        }
      }

      // Parse working storage variables
      if (currentSection === 'workingStorage' && line.match(/^\d+\s+/)) {
        const match = line.match(/^(\d+)\s+([A-Za-z0-9-]+)\s+PIC\s+([^.]+)(?:\s+VALUE\s+([^.]+))?/i);
        if (match) {
          program.data.workingStorage.push({
            level: match[1],
            name: match[2],
            picture: match[3].trim(),
            value: match[4] ? match[4].trim().replace(/['"]/g, '') : null
          });
        }
      }

      // Parse procedure division statements
      if (currentDivision === 'procedure' && line && !upperLine.includes('PROCEDURE DIVISION')) {
        program.procedure.push(line);
      }
    });

    return program;
  }

  static validateSyntax(code) {
    const errors = [];
    const lines = code.split('\n');
    
    // Check for required divisions
    const requiredDivisions = ['IDENTIFICATION DIVISION', 'PROCEDURE DIVISION'];
    requiredDivisions.forEach(division => {
      if (!code.toUpperCase().includes(division)) {
        errors.push(`Missing ${division}`);
      }
    });

    // Check for PROGRAM-ID
    if (!code.toUpperCase().includes('PROGRAM-ID')) {
      errors.push('Missing PROGRAM-ID');
    }

    // Check for proper statement termination
    lines.forEach((line, index) => {
      const trimmed = line.trim().toUpperCase();
      if (trimmed.startsWith('DISPLAY') || trimmed.startsWith('COMPUTE')) {
        if (!trimmed.endsWith('.')) {
          errors.push(`Line ${index + 1}: Statement should end with period`);
        }
      }
    });

    return errors;
  }
}