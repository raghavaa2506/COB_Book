import React, { useState, useRef, useEffect } from 'react';

const CobolEditor = ({ value, onChange, onRun, isRunning }) => {
  const [lineNumbers, setLineNumbers] = useState([]);
  const textareaRef = useRef(null);

  useEffect(() => {
    const lines = value.split('\n');
    setLineNumbers(lines.map((_, index) => index + 1));
  }, [value]);

  const handleKeyDown = (e) => {
    // Handle Tab key for indentation
    if (e.key === 'Tab') {
      e.preventDefault();
      const start = e.target.selectionStart;
      const end = e.target.selectionEnd;
      const newValue = value.substring(0, start) + '       ' + value.substring(end);
      onChange(newValue);
      
      // Set cursor position after the inserted spaces
      setTimeout(() => {
        e.target.selectionStart = e.target.selectionEnd = start + 7;
      }, 0);
    }
    
    // Handle Ctrl+Enter to run
    if (e.ctrlKey && e.key === 'Enter') {
      e.preventDefault();
      onRun();
    }
  };

  const highlightSyntax = (code) => {
    const keywords = [
      'IDENTIFICATION', 'DIVISION', 'PROGRAM-ID', 'ENVIRONMENT', 'DATA',
      'WORKING-STORAGE', 'SECTION', 'PROCEDURE', 'DISPLAY', 'COMPUTE',
      'MOVE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'IF', 'ELSE',
      'END-IF', 'PERFORM', 'UNTIL', 'VARYING', 'STOP', 'RUN', 'PIC',
      'VALUE', 'ACCEPT'
    ];

    let highlighted = code;
    keywords.forEach(keyword => {
      const regex = new RegExp(`\\b${keyword}\\b`, 'gi');
      highlighted = highlighted.replace(regex, `<span class="keyword">${keyword}</span>`);
    });

    return highlighted;
  };

  const editorStyle = {
    container: {
      position: 'relative',
      border: '1px solid #ddd',
      borderRadius: '4px',
      overflow: 'hidden'
    },
    lineNumbers: {
      position: 'absolute',
      left: 0,
      top: 0,
      width: '50px',
      backgroundColor: '#f5f5f5',
      borderRight: '1px solid #ddd',
      padding: '15px 5px',
      fontSize: '12px',
      fontFamily: 'Monaco, Consolas, "Courier New", monospace',
      color: '#666',
      textAlign: 'right',
      userSelect: 'none',
      zIndex: 1
    },
    textarea: {
      width: '100%',
      minHeight: '200px',
      padding: '15px 15px 15px 65px',
      border: 'none',
      fontSize: '14px',
      fontFamily: 'Monaco, Consolas, "Courier New", monospace',
      backgroundColor: '#fafafa',
      resize: 'vertical',
      outline: 'none',
      lineHeight: '1.4',
      tabSize: 7
    },
    runButton: {
      position: 'absolute',
      top: '10px',
      right: '10px',
      padding: '5px 10px',
      backgroundColor: '#27ae60',
      color: 'white',
      border: 'none',
      borderRadius: '3px',
      fontSize: '12px',
      cursor: 'pointer',
      zIndex: 2
    }
  };

  return (
    <div style={editorStyle.container}>
      <div style={editorStyle.lineNumbers}>
        {lineNumbers.map(num => (
          <div key={num}>{num}</div>
        ))}
      </div>
      <textarea
        ref={textareaRef}
        style={editorStyle.textarea}
        value={value}
        onChange={(e) => onChange(e.target.value)}
        onKeyDown={handleKeyDown}
        placeholder="Write your COBOL code here..."
        spellCheck={false}
      />
      <button
        style={editorStyle.runButton}
        onClick={onRun}
        disabled={isRunning}
      >
        {isRunning ? 'Running...' : 'Run (Ctrl+Enter)'}
      </button>
    </div>
  );
};

export default CobolEditor;