import React, { useState } from 'react';
import { renderMarkdown } from '../../utils/markdownRenderer';

const MarkdownEditor = ({ value, onChange }) => {
  const [isPreview, setIsPreview] = useState(false);

  const editorStyle = {
    container: {
      border: '1px solid #ddd',
      borderRadius: '4px',
      overflow: 'hidden'
    },
    tabs: {
      display: 'flex',
      backgroundColor: '#f5f5f5',
      borderBottom: '1px solid #ddd'
    },
    tab: {
      padding: '10px 20px',
      cursor: 'pointer',
      fontSize: '14px',
      borderRight: '1px solid #ddd'
    },
    activeTab: {
      backgroundColor: 'white',
      fontWeight: 'bold'
    },
    editor: {
      width: '100%',
      minHeight: '150px',
      padding: '15px',
      border: 'none',
      fontSize: '14px',
      fontFamily: 'Arial, sans-serif',
      resize: 'vertical',
      outline: 'none'
    },
    preview: {
      padding: '15px',
      minHeight: '150px',
      fontSize: '14px',
      lineHeight: '1.6',
      backgroundColor: 'white'
    }
  };

  return (
    <div style={editorStyle.container}>
      <div style={editorStyle.tabs}>
        <div
          style={{
            ...editorStyle.tab,
            ...(isPreview ? {} : editorStyle.activeTab)
          }}
          onClick={() => setIsPreview(false)}
        >
          Edit
        </div>
        <div
          style={{
            ...editorStyle.tab,
            ...(isPreview ? editorStyle.activeTab : {})
          }}
          onClick={() => setIsPreview(true)}
        >
          Preview
        </div>
      </div>
      
      {isPreview ? (
        <div
          style={editorStyle.preview}
          dangerouslySetInnerHTML={{
            __html: renderMarkdown(value)
          }}
        />
      ) : (
        <textarea
          style={editorStyle.editor}
          value={value}
          onChange={(e) => onChange(e.target.value)}
          placeholder="Write markdown text here..."
        />
      )}
    </div>
  );
};

export default MarkdownEditor;