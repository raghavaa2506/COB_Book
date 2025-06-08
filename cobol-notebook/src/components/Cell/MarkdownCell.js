import React from 'react';
import { renderMarkdown } from '../../utils/markdownRenderer';

const MarkdownCell = ({ cell, onUpdate }) => {
  return (
    <>
      <textarea
        className="markdown-editor"
        value={cell.content}
        onChange={(e) => onUpdate(cell.id, e.target.value)}
        placeholder="Write markdown text here..."
      />
      <div 
        className="markdown-preview"
        dangerouslySetInnerHTML={{
          __html: renderMarkdown(cell.content)
        }}
      />
    </>
  );
};

export default MarkdownCell;