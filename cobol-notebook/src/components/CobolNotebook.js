import React, { useState, useRef, useEffect } from 'react';

// Animation helpers
const fadeIn = {
  animation: 'fadeIn 0.7s',
};
const slideUp = {
  animation: 'slideUp 0.5s',
};
const glow = {
  boxShadow: '0 0 8px #64b5f6, 0 2px 8px rgba(52,152,219,.15)',
  transition: 'box-shadow 0.3s',
};
const keyframes = `
@keyframes fadeIn {
  from { opacity: 0;}
  to { opacity: 1;}
}
@keyframes slideUp {
  from { opacity: 0; transform: translateY(24px);}
  to { opacity: 1; transform: translateY(0);}
}
@keyframes pop {
  0% {transform:scale(.97);}
  50% {transform:scale(1.04);}
  100% {transform:scale(1);}
}
`;

const CobolNotebook = () => {
  const [cells, setCells] = useState([
    {
      id: 1,
      type: 'code',
      content: `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20) VALUE 'Hello, COBOL World!'.
       
       PROCEDURE DIVISION.
       DISPLAY WS-MESSAGE.
       STOP RUN.`,
      output: '',
      isRunning: false
    }
  ]);
  
  const [nextId, setNextId] = useState(2);
  const [isConnected, setIsConnected] = useState(false);
  const fileInputRef = useRef(null);
  const [cellAnimation, setCellAnimation] = useState(null);

  // Animation trigger for cell addition
  useEffect(() => {
    if (cellAnimation !== null) {
      setTimeout(() => setCellAnimation(null), 700);
    }
  }, [cells, cellAnimation]);

  // Mock COBOL compiler/interpreter (see your original code)
  const compileAndRun = async (code) => {
    await new Promise(resolve => setTimeout(resolve, 1000));
    const lines = code.split('\n');
    let output = '';
    const hasIdentification = lines.some(line => line.includes('IDENTIFICATION DIVISION'));
    const hasProcedure = lines.some(line => line.includes('PROCEDURE DIVISION'));
    if (!hasIdentification) {
      return { success: false, output: 'ERROR: Missing IDENTIFICATION DIVISION' };
    }
    if (!hasProcedure) {
      return { success: false, output: 'ERROR: Missing PROCEDURE DIVISION' };
    }
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
    const computeLines = lines.filter(line => 
      line.trim().toUpperCase().includes('COMPUTE')
    );
    computeLines.forEach(line => {
      const match = line.match(/COMPUTE\s+(\w+)\s*=\s*([^.]+)/i);
      if (match) {
        output += `Computed ${match[1]} = ${match[2]}\n`;
      }
    });
    if (!output) {
      output = 'Program compiled successfully. No output generated.\n';
    }
    return { success: true, output: output.trim() };
  };

  const addCell = (type = 'code', index = null) => {
    const newCell = {
      id: nextId,
      type,
      content: type === 'code' ? 
        `       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEW-PROGRAM.
       
       PROCEDURE DIVISION.
       DISPLAY 'New COBOL program'.
       STOP RUN.` : 
        '# New markdown cell\nWrite your documentation here...',
      output: '',
      isRunning: false
    };
    if (index !== null) {
      const newCells = [...cells];
      newCells.splice(index + 1, 0, newCell);
      setCells(newCells);
    } else {
      setCells([...cells, newCell]);
    }
    setNextId(nextId + 1);
    setCellAnimation(newCell.id);
  };

  const deleteCell = (id) => {
    if (cells.length > 1) {
      setCells(cells.filter(cell => cell.id !== id));
    }
  };

  const updateCell = (id, content) => {
    setCells(cells.map(cell => 
      cell.id === id ? { ...cell, content } : cell
    ));
  };

  const runCell = async (id) => {
    const cell = cells.find(c => c.id === id);
    if (!cell || cell.type !== 'code') return;
    setCells(cells.map(c => 
      c.id === id ? { ...c, isRunning: true, output: 'Compiling...' } : c
    ));
    try {
      const result = await compileAndRun(cell.content);
      setCells(cells.map(c => 
        c.id === id ? { 
          ...c, 
          isRunning: false, 
          output: result.success ? result.output : result.output 
        } : c
      ));
    } catch (error) {
      setCells(cells.map(c => 
        c.id === id ? { 
          ...c, 
          isRunning: false, 
          output: `Error: ${error.message}` 
        } : c
      ));
    }
  };

  const runAllCells = async () => {
    for (const cell of cells.filter(c => c.type === 'code')) {
      await runCell(cell.id);
      await new Promise(resolve => setTimeout(resolve, 400));
    }
  };

  const saveNotebook = () => {
    const notebook = {
      cells: cells,
      metadata: {
        created: new Date().toISOString(),
        cobolVersion: '85',
        title: 'COBOL Notebook'
      }
    };
    const blob = new Blob([JSON.stringify(notebook, null, 2)], {
      type: 'application/json'
    });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'cobol-notebook.json';
    a.click();
    URL.revokeObjectURL(url);
  };

  const loadNotebook = (event) => {
    const file = event.target.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = (e) => {
      try {
        const notebook = JSON.parse(e.target.result);
        if (notebook.cells && Array.isArray(notebook.cells)) {
          setCells(notebook.cells);
          setNextId(Math.max(...notebook.cells.map(c => c.id)) + 1);
        }
      } catch (error) {
        alert('Error loading notebook: ' + error.message);
      }
    };
    reader.readAsText(file);
    event.target.value = '';
  };

  const renderMarkdown = (content) => {
    return content
      .replace(/^# (.*$)/gm, '<h1>$1</h1>')
      .replace(/^## (.*$)/gm, '<h2>$1</h2>')
      .replace(/^### (.*$)/gm, '<h3>$1</h3>')
      .replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>')
      .replace(/\*(.*?)\*/g, '<em>$1</em>')
      .replace(/`(.*?)`/g, '<code>$1</code>')
      .replace(/\n/g, '<br>');
  };

  // --- UI Styling ---
  const styles = {
    container: {
      fontFamily: 'Inter, Arial, sans-serif',
      background: 'linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%)',
      minHeight: '100vh',
      padding: '0',
      transition: 'background .5s',
    },
    header: {
      background: 'linear-gradient(90deg,#283e51 0%,#485563 100%)',
      color: 'white',
      padding: '18px 0 18px 0',
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      boxShadow: '0 2px 12px rgba(44,62,80,0.12)',
      position: 'sticky',
      top: 0,
      zIndex: 10
    },
    title: {
      margin: '0 32px',
      fontSize: '2.1rem',
      fontWeight: 700,
      letterSpacing: '1.2px',
      display: 'flex',
      alignItems: 'center',
      gap: '10px'
    },
    logo: {
      width: 35,
      height: 35,
      borderRadius: '50%',
      background: 'linear-gradient(135deg, #4fc3f7 20%, #1976d2 100%)',
      display: 'inline-block',
      marginRight: '12px',
      boxShadow: '0 4px 12px #1976d260'
    },
    toolbar: {
      display: 'flex',
      gap: '12px',
      alignItems: 'center',
      marginRight: '36px'
    },
    button: {
      padding: '10px 22px',
      border: 'none',
      borderRadius: '22px',
      cursor: 'pointer',
      fontSize: '15px',
      fontWeight: 600,
      letterSpacing: '.04em',
      transition: 'background 0.2s, transform 0.13s',
      outline: 'none',
      boxShadow: '0 2px 8px #0001'
    },
    primaryButton: {
      background: 'linear-gradient(90deg,#6dd5ed 0,#2193b0 100%)',
      color: 'white',
    },
    successButton: {
      background: 'linear-gradient(90deg,#56ab2f,#a8e063)',
      color: 'white',
    },
    dangerButton: {
      background: 'linear-gradient(90deg,#ff5858,#f09819)',
      color: 'white',
    },
    secondaryButton: {
      background: 'linear-gradient(90deg,#d3cce3,#e9e4f0)',
      color: '#34495e'
    },
    main: {
      maxWidth: '900px',
      margin: '0 auto',
      padding: '36px 10px 110px 10px'
    },
    cell: {
      background: 'linear-gradient(135deg,#fff 65%,#e3f5ff 100%)',
      border: '1.5px solid #e0e5ec',
      borderRadius: '18px',
      marginBottom: '40px',
      overflow: 'hidden',
      boxShadow: '0 6px 24px 0 rgba(52, 152, 219, 0.06),0 1.5px 4px #1976d230',
      position: 'relative',
      ...slideUp,
    },
    cellHeader: {
      background: 'linear-gradient(90deg,#f8fafc 50%,#e3f5ff 100%)',
      padding: '13px 23px',
      borderBottom: '1px solid #f0f0f8',
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center'
    },
    cellType: {
      fontSize: '13px',
      fontWeight: 700,
      color: '#1976d2',
      textTransform: 'uppercase',
      letterSpacing: '.05em'
    },
    cellActions: {
      display: 'flex',
      gap: '7px'
    },
    smallButton: {
      padding: '4.5px 13px',
      fontSize: '12.5px',
      border: 'none',
      borderRadius: '12px',
      cursor: 'pointer',
      fontWeight: 600,
      transition: 'background 0.17s, box-shadow .18s',
      outline: 'none',
      boxShadow: '0 1.2px 6px #1976d230'
    },
    codeEditor: {
      width: '100%',
      minHeight: '145px',
      padding: '19px',
      border: 'none',
      fontSize: '15px',
      fontFamily: 'Fira Mono, Monaco, Consolas, "Courier New", monospace',
      background: 'linear-gradient(90deg,#f8fafc 60%,#e3f5ff 100%)',
      resize: 'vertical',
      outline: 'none',
      borderRadius: '0 0 0 0',
      color: '#23395d',
      transition: 'background 0.15s',
      ...fadeIn
    },
    markdownEditor: {
      width: '100%',
      minHeight: '88px',
      padding: '17px',
      border: 'none',
      fontSize: '15px',
      fontFamily: 'Inter, Arial, sans-serif',
      resize: 'vertical',
      outline: 'none',
      background: '#f8fafc',
      color: '#34495e',
      ...fadeIn
    },
    output: {
      background: 'linear-gradient(90deg,#0f2027 0%, #203a43 100%)',
      color: '#f9f9f9',
      padding: '18px 22px 12px 22px',
      fontFamily: 'Fira Mono, Monaco, Consolas, "Courier New", monospace',
      fontSize: '14px',
      whiteSpace: 'pre-wrap',
      borderTop: '1px solid #e0e5ec',
      minHeight: '39px',
      borderBottomLeftRadius: '18px',
      borderBottomRightRadius: '18px',
      ...fadeIn
    },
    markdownPreview: {
      padding: '19px',
      fontSize: '15.5px',
      lineHeight: '1.7',
      color: '#23395d',
      background: 'none',
      ...fadeIn
    },
    statusBar: {
      background: 'linear-gradient(90deg,#283e51 0%,#485563 100%)',
      color: 'white',
      padding: '15px 32px',
      position: 'fixed',
      bottom: 0,
      left: 0,
      right: 0,
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      fontSize: '14px',
      letterSpacing: '.02em',
      boxShadow: '0 -2px 16px #283e5110',
      zIndex: 50
    },
    hiddenInput: { display: 'none' },
    runningIndicator: {
      color: '#ffeb3b',
      fontSize: '13px',
      fontWeight: 'bold',
      marginBottom: '6px',
      letterSpacing: '.04em',
      animation: 'fadeIn 0.8s'
    },
    cellGlow: {
      boxShadow: '0 0 0 4px #2196f340,0 0 16px #2196f360',
      animation: 'pop 0.35s'
    },
    cellFade: {
      animation: 'fadeIn 0.7s'
    }
  };

  // --- Render ---
  return (
    <div style={styles.container}>
      {/* Animation Keyframes */}
      <style>{keyframes}</style>
      <header style={styles.header}>
        <span style={styles.title}>
          <span style={styles.logo}></span>
          COBOL Notebook
        </span>
        <div style={styles.toolbar}>
          <button 
            style={{...styles.button, ...styles.successButton}}
            onClick={runAllCells}
          >
            <span role="img" aria-label="run all">⚡</span> Run All
          </button>
          <button 
            style={{...styles.button, ...styles.primaryButton}}
            onClick={() => addCell('code')}
          >
            + Code
          </button>
          <button 
            style={{...styles.button, ...styles.primaryButton}}
            onClick={() => addCell('markdown')}
          >
            + Text
          </button>
          <button 
            style={{...styles.button, ...styles.secondaryButton}}
            onClick={saveNotebook}
          >
            <span role="img" aria-label="save">💾</span> Save
          </button>
          <button 
            style={{...styles.button, ...styles.secondaryButton}}
            onClick={() => fileInputRef.current.click()}
          >
            <span role="img" aria-label="load">📂</span> Load
          </button>
        </div>
      </header>

      <input
        ref={fileInputRef}
        type="file"
        accept=".json"
        onChange={loadNotebook}
        style={styles.hiddenInput}
      />

      <main style={styles.main}>
        {cells.map((cell, index) => (
          <div
            key={cell.id}
            style={{
              ...styles.cell,
              ...(cellAnimation === cell.id ? styles.cellGlow : {}),
            }}
          >
            <div style={styles.cellHeader}>
              <span style={styles.cellType}>
                {cell.type === 'code' ? (
                  <>
                    <span role="img" aria-label="cobol" style={{marginRight: 4}}>💻</span>
                    COBOL Code
                  </>
                ) : (
                  <>
                    <span role="img" aria-label="doc" style={{marginRight: 4}}>📝</span>
                    Markdown Text
                  </>
                )}
              </span>
              <div style={styles.cellActions}>
                {cell.type === 'code' && (
                  <button
                    style={{...styles.smallButton, ...styles.successButton}}
                    onClick={() => runCell(cell.id)}
                    disabled={cell.isRunning}
                  >
                    {cell.isRunning ? (
                      <span>
                        <span className="cell-dot" style={{color:'#ffeb3b'}}>●</span> Running...
                      </span>
                    ) : (
                      <span>
                        <span style={{marginRight:2}} role="img" aria-label="run">▶️</span> Run
                      </span>
                    )}
                  </button>
                )}
                <button
                  style={{...styles.smallButton, ...styles.primaryButton}}
                  onClick={() => addCell('code', index)}
                  title="Add Code Cell Below"
                >
                  + Code
                </button>
                <button
                  style={{...styles.smallButton, ...styles.primaryButton}}
                  onClick={() => addCell('markdown', index)}
                  title="Add Markdown Cell Below"
                >
                  + Text
                </button>
                <button
                  style={{...styles.smallButton, ...styles.dangerButton}}
                  onClick={() => deleteCell(cell.id)}
                  title="Delete Cell"
                >
                  <span role="img" aria-label="delete">🗑️</span>
                </button>
              </div>
            </div>
            
            {cell.type === 'code' ? (
              <>
                <textarea
                  style={styles.codeEditor}
                  value={cell.content}
                  onChange={(e) => updateCell(cell.id, e.target.value)}
                  placeholder="Write your COBOL code here..."
                  spellCheck={false}
                  autoFocus={cellAnimation === cell.id}
                />
                {(cell.output || cell.isRunning) && (
                  <div style={styles.output}>
                    {cell.isRunning && <div style={styles.runningIndicator}>● Running...</div>}
                    {cell.output}
                  </div>
                )}
              </>
            ) : (
              <>
                <textarea
                  style={styles.markdownEditor}
                  value={cell.content}
                  onChange={(e) => updateCell(cell.id, e.target.value)}
                  placeholder="Write markdown text here..."
                />
                <div 
                  style={styles.markdownPreview}
                  dangerouslySetInnerHTML={{
                    __html: renderMarkdown(cell.content)
                  }}
                />
              </>
            )}
          </div>
        ))}
      </main>

      <div style={styles.statusBar}>
        <div>
          <span role="img" aria-label="logo" style={{fontSize:'17px', verticalAlign:'middle'}}>💾</span>
          &nbsp;COBOL Notebook v1.0 &nbsp; | &nbsp;
          <span role="img" aria-label="box" style={{fontSize:'14px', verticalAlign:'middle'}}>📦</span>
          &nbsp;{cells.length} cells &nbsp; | &nbsp; 
          <span role="img" aria-label="bolt" style={{fontSize:'14px',verticalAlign:'middle'}}>⚡</span>
          &nbsp;Ready
        </div>
        <div>
          <span role="img" aria-label="link" style={{fontSize:'14px', verticalAlign:'middle'}}>🔗</span>
          &nbsp;Status: <span style={{fontWeight:600}}>{isConnected ? 'Connected' : 'Local Mode'}</span>
        </div>
      </div>
    </div>
  );
};

export default CobolNotebook;