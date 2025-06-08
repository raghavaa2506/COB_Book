export class CobolDebugger {
  static trace(program, input = {}) {
    const trace = [];
    const variables = { ...input };
    
    program.procedure.forEach((statement, index) => {
      const upperStatement = statement.toUpperCase();
      
      if (upperStatement.includes('DISPLAY')) {
        const match = statement.match(/DISPLAY\s+([^.]+)/i);
        if (match) {
          const value = this.resolveValue(match[1].trim(), variables, program);
          trace.push({
            step: index + 1,
            statement,
            action: 'DISPLAY',
            value,
            variables: { ...variables }
          });
        }
      }
      
      if (upperStatement.includes('MOVE')) {
        const match = statement.match(/MOVE\s+([^.]+)\s+TO\s+([^.]+)/i);
        if (match) {
          const source = this.resolveValue(match[1].trim(), variables, program);
          const target = match[2].trim();
          variables[target] = source;
          trace.push({
            step: index + 1,
            statement,
            action: 'MOVE',
            source,
            target,
            variables: { ...variables }
          });
        }
      }
    });
    
    return trace;
  }
  
  static resolveValue(value, variables, program) {
    // Handle string literals
    if (value.startsWith("'") && value.endsWith("'")) {
      return value.slice(1, -1);
    }
    
    // Handle variables
    if (variables[value]) {
      return variables[value];
    }
    
    // Look up in working storage
    const wsVar = program.data.workingStorage.find(v => v.name === value);
    if (wsVar && wsVar.value) {
      return wsVar.value;
    }
    
    return value;
  }
}