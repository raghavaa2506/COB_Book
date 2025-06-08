import React from 'react';
import CodeCell from './CodeCell';
import MarkdownCell from './MarkdownCell';

const Cell = ({ cell, onUpdate, onRun, onDelete, onAddCell, index }) => {
  const handleAddCell = (type) => {
    onAddCell(type, index);
  };

  return (
    <div className="cell">
      <div className="cell-header">
        <span className="cell-type">
          {cell.type === 'code' ? 'COBOL Code' : 'Markdown Text'}
        </span>
        <div className="cell-actions">
          {cell.type === 'code' && (
            <button
              className="btn btn-success btn-small"
              onClick={() => onRun(cell.id)}
              disabled={cell.isRunning}
            >
              {cell.isRunning ? 'Running...' : 'Run'}
            </button>
          )}
          <button
            className="btn btn-primary btn-small"
            onClick={() => handleAddCell('code')}
          >
            + Code
          </button>
          <button
            className="btn btn-primary btn-small"
            onClick={() => handleAddCell('markdown')}
          >
            + Text
          </button>
          <button
            className="btn btn-danger btn-small"
            onClick={() => onDelete(cell.id)}
          >
            Delete
          </button>
        </div>
      </div>
      
      {cell.type === 'code' ? (
        <CodeCell 
          cell={cell} 
          onUpdate={onUpdate}
        />
      ) : (
        <MarkdownCell 
          cell={cell} 
          onUpdate={onUpdate}
        />
      )}
    </div>
  );
};

export default Cell;