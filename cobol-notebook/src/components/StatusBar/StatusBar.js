import React from 'react';

const StatusBar = ({ cellCount, isConnected }) => {
  return (
    <div className="status-bar">
      <div>
        COBOL Notebook v1.0 | {cellCount} cells | Ready
      </div>
      <div>
        Status: {isConnected ? 'Connected' : 'Local Mode'}
      </div>
    </div>
  );
};

export default StatusBar;