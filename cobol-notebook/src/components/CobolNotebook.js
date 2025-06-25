import React, { useState, useRef, useEffect } from 'react';

// Enhanced Animation helpers with more sophisticated effects
const animations = {
  fadeIn: {
    animation: 'fadeIn 0.8s cubic-bezier(0.4, 0, 0.2, 1)',
  },
  slideUp: {
    animation: 'slideUp 0.6s cubic-bezier(0.4, 0, 0.2, 1)',
  },
  scaleIn: {
    animation: 'scaleIn 0.5s cubic-bezier(0.4, 0, 0.2, 1)',
  },
  glow: {
    boxShadow: '0 0 20px rgba(100, 181, 246, 0.4), 0 8px 32px rgba(52, 152, 219, 0.2)',
    transition: 'all 0.3s cubic-bezier(0.4, 0, 0.2, 1)',
  },
  pulse: {
    animation: 'pulse 2s infinite',
  }
};

const keyframes = `
@keyframes fadeIn {
  from { opacity: 0; transform: translateY(10px); }
  to { opacity: 1; transform: translateY(0); }
}
@keyframes slideUp {
  from { opacity: 0; transform: translateY(40px) scale(0.95); }
  to { opacity: 1; transform: translateY(0) scale(1); }
}
@keyframes scaleIn {
  from { opacity: 0; transform: scale(0.9); }
  to { opacity: 1; transform: scale(1); }
}
@keyframes pop {
  0% { transform: scale(0.95); }
  50% { transform: scale(1.05); }
  100% { transform: scale(1); }
}
@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.7; }
}
@keyframes shimmer {
  0% { background-position: -200px 0; }
  100% { background-position: calc(200px + 100%) 0; }
}
@keyframes float {
  0%, 100% { transform: translateY(0px); }
  50% { transform: translateY(-10px); }
}
@keyframes spin {
  from { transform: rotate(0deg); }
  to { transform: rotate(360deg); }
}
@keyframes wave {
  0%, 100% { transform: rotate(0deg); }
  25% { transform: rotate(5deg); }
  75% { transform: rotate(-5deg); }
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
  const [theme, setTheme] = useState('light');
  const [isLoading, setIsLoading] = useState(false);

  // Animation trigger for cell addition
  useEffect(() => {
    if (cellAnimation !== null) {
      setTimeout(() => setCellAnimation(null), 800);
    }
  }, [cells, cellAnimation]);

  // Mock COBOL compiler/interpreter
  const compileAndRun = async (code) => {
    await new Promise(resolve => setTimeout(resolve, 1500));
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
    setIsLoading(true);
    for (const cell of cells.filter(c => c.type === 'code')) {
      await runCell(cell.id);
      await new Promise(resolve => setTimeout(resolve, 400));
    }
    setIsLoading(false);
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
      .replace(/^# (.*$)/gm, '<h1 style="color: #2c3e50; margin-bottom: 16px; font-size: 28px;">$1</h1>')
      .replace(/^## (.*$)/gm, '<h2 style="color: #34495e; margin-bottom: 12px; font-size: 22px;">$1</h2>')
      .replace(/^### (.*$)/gm, '<h3 style="color: #5d6d7e; margin-bottom: 10px; font-size: 18px;">$1</h3>')
      .replace(/\*\*(.*?)\*\*/g, '<strong style="color: #2980b9;">$1</strong>')
      .replace(/\*(.*?)\*/g, '<em style="color: #8e44ad;">$1</em>')
      .replace(/`(.*?)`/g, '<code style="background: #f8f9fa; padding: 2px 6px; border-radius: 4px; color: #e74c3c; font-family: monospace;">$1</code>')
      .replace(/\n/g, '<br>');
  };

  const styles = {
    container: {
      fontFamily: '"Inter", "Segoe UI", system-ui, sans-serif',
      background: theme === 'light' 
        ? 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)'
        : 'linear-gradient(135deg, #1e3c72 0%, #2a5298 100%)',
      minHeight: '100vh',
      transition: 'all 0.5s cubic-bezier(0.4, 0, 0.2, 1)',
      position: 'relative',
      overflow: 'hidden',
    },
    backgroundPattern: {
      position: 'absolute',
      top: 0,
      left: 0,
      right: 0,
      bottom: 0,
      backgroundImage: `radial-gradient(circle at 25% 25%, rgba(255,255,255,0.1) 0%, transparent 50%),
                        radial-gradient(circle at 75% 75%, rgba(255,255,255,0.1) 0%, transparent 50%)`,
      animation: 'float 6s ease-in-out infinite',
      zIndex: 0,
    },
    header: {
      background: 'rgba(255, 255, 255, 0.1)',
      backdropFilter: 'blur(20px)',
      WebkitBackdropFilter: 'blur(20px)',
      border: '1px solid rgba(255, 255, 255, 0.2)',
      color: 'white',
      padding: '20px 0',
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      position: 'sticky',
      top: 0,
      zIndex: 100,
      boxShadow: '0 8px 32px rgba(0, 0, 0, 0.1)',
      ...animations.fadeIn,
    },
    title: {
      margin: '0 40px',
      fontSize: '2.5rem',
      fontWeight: 800,
      letterSpacing: '2px',
      display: 'flex',
      alignItems: 'center',
      gap: '15px',
      textShadow: '0 2px 20px rgba(0,0,0,0.3)',
    },
    logo: {
      width: 45,
      height: 45,
      borderRadius: '50%',
      background: 'linear-gradient(135deg, #ff6b6b 0%, #ee5a24 100%)',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      fontSize: '20px',
      boxShadow: '0 8px 32px rgba(255, 107, 107, 0.4)',
      animation: 'wave 3s ease-in-out infinite',
    },
    toolbar: {
      display: 'flex',
      gap: '15px',
      alignItems: 'center',
      marginRight: '40px'
    },
    button: {
      padding: '12px 24px',
      border: 'none',
      borderRadius: '50px',
      cursor: 'pointer',
      fontSize: '14px',
      fontWeight: 600,
      letterSpacing: '0.5px',
      transition: 'all 0.3s cubic-bezier(0.4, 0, 0.2, 1)',
      outline: 'none',
      position: 'relative',
      overflow: 'hidden',
      textTransform: 'uppercase',
    },
    primaryButton: {
      background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
      color: 'white',
      boxShadow: '0 4px 15px rgba(102, 126, 234, 0.4)',
    },
    successButton: {
      background: 'linear-gradient(135deg, #56ab2f 0%, #a8e063 100%)',
      color: 'white',
      boxShadow: '0 4px 15px rgba(86, 171, 47, 0.4)',
    },
    dangerButton: {
      background: 'linear-gradient(135deg, #ff512f 0%, #f09819 100%)',
      color: 'white',
      boxShadow: '0 4px 15px rgba(255, 81, 47, 0.4)',
    },
    secondaryButton: {
      background: 'rgba(255, 255, 255, 0.2)',
      backdropFilter: 'blur(10px)',
      color: 'white',
      border: '1px solid rgba(255, 255, 255, 0.3)',
      boxShadow: '0 4px 15px rgba(255, 255, 255, 0.1)',
    },
    main: {
      maxWidth: '1000px',
      margin: '0 auto',
      padding: '40px 20px 120px 20px',
      position: 'relative',
      zIndex: 1,
    },
    cell: {
      background: 'rgba(255, 255, 255, 0.95)',
      backdropFilter: 'blur(20px)',
      WebkitBackdropFilter: 'blur(20px)',
      border: '1px solid rgba(255, 255, 255, 0.3)',
      borderRadius: '24px',
      marginBottom: '30px',
      overflow: 'hidden',
      boxShadow: '0 20px 40px rgba(0, 0, 0, 0.1), 0 1px 3px rgba(0, 0, 0, 0.1)',
      position: 'relative',
      transition: 'all 0.3s cubic-bezier(0.4, 0, 0.2, 1)',
      ...animations.slideUp,
    },
    cellHover: {
      transform: 'translateY(-5px)',
      boxShadow: '0 25px 50px rgba(0, 0, 0, 0.15), 0 5px 15px rgba(0, 0, 0, 0.1)',
    },
    cellHeader: {
      background: 'linear-gradient(135deg, rgba(255,255,255,0.8) 0%, rgba(248,250,252,0.9) 100%)',
      backdropFilter: 'blur(10px)',
      padding: '20px 30px',
      borderBottom: '1px solid rgba(255, 255, 255, 0.3)',
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center'
    },
    cellType: {
      fontSize: '14px',
      fontWeight: 700,
      background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
      WebkitBackgroundClip: 'text',
      WebkitTextFillColor: 'transparent',
      textTransform: 'uppercase',
      letterSpacing: '1px',
      display: 'flex',
      alignItems: 'center',
      gap: '8px'
    },
    cellActions: {
      display: 'flex',
      gap: '10px'
    },
    smallButton: {
      padding: '8px 16px',
      fontSize: '12px',
      border: 'none',
      borderRadius: '20px',
      cursor: 'pointer',
      fontWeight: 600,
      transition: 'all 0.2s cubic-bezier(0.4, 0, 0.2, 1)',
      outline: 'none',
      textTransform: 'uppercase',
      letterSpacing: '0.5px',
    },
    codeEditor: {
      width: '100%',
      minHeight: '200px',
      padding: '30px',
      border: 'none',
      fontSize: '15px',
      fontFamily: '"Fira Code", "JetBrains Mono", "Cascadia Code", monospace',
      background: 'linear-gradient(135deg, #f8fafc 0%, #e3f5ff 100%)',
      resize: 'vertical',
      outline: 'none',
      color: '#2d3748',
      lineHeight: '1.6',
      transition: 'all 0.3s ease',
      ...animations.fadeIn
    },
    markdownEditor: {
      width: '100%',
      minHeight: '120px',
      padding: '25px',
      border: 'none',
      fontSize: '16px',
      fontFamily: '"Inter", system-ui, sans-serif',
      resize: 'vertical',
      outline: 'none',
      background: 'linear-gradient(135deg, #f8fafc 0%, #ffffff 100%)',
      color: '#2d3748',
      lineHeight: '1.7',
      ...animations.fadeIn
    },
    output: {
      background: 'linear-gradient(135deg, #1a202c 0%, #2d3748 100%)',
      color: '#e2e8f0',
      padding: '25px 30px',
      fontFamily: '"Fira Code", monospace',
      fontSize: '14px',
      whiteSpace: 'pre-wrap',
      borderTop: '1px solid rgba(255, 255, 255, 0.1)',
      minHeight: '60px',
      position: 'relative',
      ...animations.fadeIn
    },
    markdownPreview: {
      padding: '30px',
      fontSize: '16px',
      lineHeight: '1.8',
      color: '#2d3748',
      background: 'linear-gradient(135deg, #ffffff 0%, #f8fafc 100%)',
      ...animations.fadeIn
    },
    statusBar: {
      background: 'rgba(26, 32, 44, 0.95)',
      backdropFilter: 'blur(20px)',
      WebkitBackdropFilter: 'blur(20px)',
      color: 'white',
      padding: '20px 40px',
      position: 'fixed',
      bottom: 0,
      left: 0,
      right: 0,
      display: 'flex',
      justifyContent: 'space-between',
      alignItems: 'center',
      fontSize: '14px',
      letterSpacing: '0.5px',
      border: '1px solid rgba(255, 255, 255, 0.1)',
      borderBottom: 'none',
      zIndex: 100
    },
    hiddenInput: { display: 'none' },
    runningIndicator: {
      color: '#ffd700',
      fontSize: '14px',
      fontWeight: 'bold',
      marginBottom: '10px',
      letterSpacing: '0.5px',
      display: 'flex',
      alignItems: 'center',
      gap: '8px',
      ...animations.pulse
    },
    loadingSpinner: {
      width: '16px',
      height: '16px',
      border: '2px solid rgba(255, 215, 0, 0.3)',
      borderTop: '2px solid #ffd700',
      borderRadius: '50%',
      animation: 'spin 1s linear infinite',
    },
    cellGlow: {
      boxShadow: '0 0 0 3px rgba(102, 126, 234, 0.3), 0 25px 50px rgba(102, 126, 234, 0.2)',
      animation: 'pop 0.4s cubic-bezier(0.4, 0, 0.2, 1)',
      transform: 'scale(1.02)',
    },
    shimmerButton: {
      background: 'linear-gradient(90deg, transparent, rgba(255,255,255,0.4), transparent)',
      backgroundSize: '200px 100%',
      animation: 'shimmer 2s infinite',
    }
  };

  const ButtonHover = ({ children, style, ...props }) => {
    const [isHovered, setIsHovered] = useState(false);
    
    return (
      <button
        {...props}
        style={{
          ...style,
          transform: isHovered ? 'translateY(-2px) scale(1.05)' : 'translateY(0) scale(1)',
          boxShadow: isHovered 
            ? `${style.boxShadow}, 0 8px 25px rgba(0,0,0,0.15)` 
            : style.boxShadow,
        }}
        onMouseEnter={() => setIsHovered(true)}
        onMouseLeave={() => setIsHovered(false)}
      >
        {children}
      </button>
    );
  };

  const CellContainer = ({ children, cell }) => {
    const [isHovered, setIsHovered] = useState(false);
    
    return (
      <div
        style={{
          ...styles.cell,
          ...(cellAnimation === cell.id ? styles.cellGlow : {}),
          ...(isHovered ? styles.cellHover : {}),
        }}
        onMouseEnter={() => setIsHovered(true)}
        onMouseLeave={() => setIsHovered(false)}
      >
        {children}
      </div>
    );
  };

  return (
    <div style={styles.container}>
      <div style={styles.backgroundPattern}></div>
      <style>{keyframes}</style>
      
      <header style={styles.header}>
        <span style={styles.title}>
          <div style={styles.logo}>
            💻
          </div>
          COBOL Notebook
        </span>
        <div style={styles.toolbar}>
          <ButtonHover 
            style={{...styles.button, ...styles.successButton}}
            onClick={runAllCells}
            disabled={isLoading}
          >
            {isLoading ? (
              <>
                <div style={styles.loadingSpinner}></div>
                Running...
              </>
            ) : (
              <>⚡ Run All</>
            )}
          </ButtonHover>
          <ButtonHover 
            style={{...styles.button, ...styles.primaryButton}}
            onClick={() => addCell('code')}
          >
            + Code
          </ButtonHover>
          <ButtonHover 
            style={{...styles.button, ...styles.primaryButton}}
            onClick={() => addCell('markdown')}
          >
            + Text
          </ButtonHover>
          <ButtonHover 
            style={{...styles.button, ...styles.secondaryButton}}
            onClick={saveNotebook}
          >
            💾 Save
          </ButtonHover>
          <ButtonHover 
            style={{...styles.button, ...styles.secondaryButton}}
            onClick={() => fileInputRef.current.click()}
          >
            📂 Load
          </ButtonHover>
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
          <CellContainer key={cell.id} cell={cell}>
            <div style={styles.cellHeader}>
              <span style={styles.cellType}>
                {cell.type === 'code' ? (
                  <>
                    <span style={{fontSize: '16px'}}>⚙️</span>
                    COBOL Code
                  </>
                ) : (
                  <>
                    <span style={{fontSize: '16px'}}>📝</span>
                    Markdown Text
                  </>
                )}
              </span>
              <div style={styles.cellActions}>
                {cell.type === 'code' && (
                  <ButtonHover
                    style={{...styles.smallButton, ...styles.successButton}}
                    onClick={() => runCell(cell.id)}
                    disabled={cell.isRunning}
                  >
                    {cell.isRunning ? (
                      <>
                        <div style={styles.loadingSpinner}></div>
                        Running
                      </>
                    ) : (
                      <>▶️ Run</>
                    )}
                  </ButtonHover>
                )}
                <ButtonHover
                  style={{...styles.smallButton, ...styles.primaryButton}}
                  onClick={() => addCell('code', index)}
                  title="Add Code Cell Below"
                >
                  + Code
                </ButtonHover>
                <ButtonHover
                  style={{...styles.smallButton, ...styles.primaryButton}}
                  onClick={() => addCell('markdown', index)}
                  title="Add Markdown Cell Below"
                >
                  + Text
                </ButtonHover>
                <ButtonHover
                  style={{...styles.smallButton, ...styles.dangerButton}}
                  onClick={() => deleteCell(cell.id)}
                  title="Delete Cell"
                >
                  🗑️
                </ButtonHover>
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
                    {cell.isRunning && (
                      <div style={styles.runningIndicator}>
                        <div style={styles.loadingSpinner}></div>
                        Compiling and executing...
                      </div>
                    )}
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
                  autoFocus={cellAnimation === cell.id}
                />
                <div 
                  style={styles.markdownPreview}
                  dangerouslySetInnerHTML={{
                    __html: renderMarkdown(cell.content)
                  }}
                />
              </>
            )}
          </CellContainer>
        ))}
      </main>

      <div style={styles.statusBar}>
        <div style={{display: 'flex', alignItems: 'center', gap: '20px'}}>
          <span style={{fontSize: '18px'}}>💾</span>
          <strong>COBOL Notebook v2.0</strong>
          <span style={{opacity: 0.7}}>|</span>
          <span>📦 {cells.length} cells</span>
          <span style={{opacity: 0.7}}>|</span>
          <span style={{display: 'flex', alignItems: 'center', gap: '8px'}}>
            ⚡ <strong style={{color: '#4ade80'}}>Ready</strong>
          </span>
        </div>
        <div style={{display: 'flex', alignItems: 'center', gap: '8px'}}>
          <span style={{fontSize: '14px'}}>🔗</span>
          Status: <strong style={{color: isConnected ? '#4ade80' : '#fbbf24'}}>
            {isConnected ? 'Connected' : 'Local Mode'}
          </strong>
        </div>
      </div>
    </div>
  );
};

export default CobolNotebook;