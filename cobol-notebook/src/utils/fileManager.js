export const saveNotebook = (cells) => {
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

export const loadNotebook = (file, callback) => {
  const reader = new FileReader();
  reader.onload = (e) => {
    try {
      const notebook = JSON.parse(e.target.result);
      if (notebook.cells && Array.isArray(notebook.cells)) {
        callback(notebook.cells);
      }
    } catch (error) {
      alert('Error loading notebook: ' + error.message);
    }
  };
  reader.readAsText(file);
};