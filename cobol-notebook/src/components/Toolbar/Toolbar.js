import React from 'react';

const Toolbar = ({ onRunAll, onAddCell, onSave, onLoad }) => {
  return (
    <div className="toolbar">
      <button 
        className="btn btn-success"
        onClick={onRunAll}
      >
        Run All
      </button>
      <button 
        className="btn btn-primary"
        onClick={() => onAddCell('code')}
      >
        + Code
      </button>
      <button 
        className="btn btn-primary"
        onClick={() => onAddCell('markdown')}
      >
        + Text
      </button>
      <button 
        className="btn btn-secondary"
        onClick={onSave}
      >
        Save
      </button>
      <button 
        className="btn btn-secondary"
        onClick={onLoad}
      >
        Load
      </button>
    </div>
  );
};

export default Toolbar;