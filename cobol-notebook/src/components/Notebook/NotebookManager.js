import React, { useState, useEffect } from 'react';

const NotebookManager = ({ onLoad, onSave, cells }) => {
  const [notebooks, setNotebooks] = useState([]);
  const [currentNotebook, setCurrentNotebook] = useState(null);

  // Auto-save functionality
  useEffect(() => {
    const autoSave = () => {
      if (currentNotebook && cells.length > 0) {
        const notebook = {
          id: currentNotebook,
          cells: cells,
          lastModified: new Date().toISOString(),
          title: `Notebook ${currentNotebook}`
        };
        localStorage.setItem(`notebook_${currentNotebook}`, JSON.stringify(notebook));
      }
    };

    const timer = setTimeout(autoSave, 30000); // Auto-save every 30 seconds
    return () => clearTimeout(timer);
  }, [cells, currentNotebook]);

  const createNewNotebook = () => {
    const id = Date.now().toString();
    setCurrentNotebook(id);
    const newNotebooks = [...notebooks, {
      id,
      title: `New Notebook ${notebooks.length + 1}`,
      created: new Date().toISOString()
    }];
    setNotebooks(newNotebooks);
    localStorage.setItem('notebooks', JSON.stringify(newNotebooks));
  };

  const loadNotebook = (id) => {
    const saved = localStorage.getItem(`notebook_${id}`);
    if (saved) {
      const notebook = JSON.parse(saved);
      onLoad(notebook.cells);
      setCurrentNotebook(id);
    }
  };

  const deleteNotebook = (id) => {
    const newNotebooks = notebooks.filter(nb => nb.id !== id);
    setNotebooks(newNotebooks);
    localStorage.removeItem(`notebook_${id}`);
    localStorage.setItem('notebooks', JSON.stringify(newNotebooks));
    
    if (currentNotebook === id) {
      setCurrentNotebook(null);
    }
  };

  return (
    <div className="notebook-manager">
      <h3>Notebook Manager</h3>
      <button onClick={createNewNotebook} className="btn btn-primary">
        New Notebook
      </button>
      
      <div className="notebook-list">
        {notebooks.map(notebook => (
          <div key={notebook.id} className="notebook-item">
            <span>{notebook.title}</span>
            <div>
              <button onClick={() => loadNotebook(notebook.id)} className="btn btn-small">
                Load
              </button>
              <button onClick={() => deleteNotebook(notebook.id)} className="btn btn-danger btn-small">
                Delete
              </button>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};

export default NotebookManager;