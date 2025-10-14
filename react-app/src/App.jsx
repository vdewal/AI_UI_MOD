import React, { useState } from 'react';
import AddAccount from './components/AddAccount';
import './App.css';

function App() {
  const [showAddAccount, setShowAddAccount] = useState(false);
  const [customerId] = useState(12345); // Mock customer ID

  const handleOpenAddAccount = () => {
    setShowAddAccount(true);
  };

  const handleCloseAddAccount = () => {
    setShowAddAccount(false);
  };

  return (
    <div className="app">
      <header className="app-header">
        <h1>Test Bank Management System</h1>
        <p>Customer ID: {customerId}</p>
      </header>

      <main className="app-main">
        <div className="welcome-section">
          <h2>Welcome to Test Bank</h2>
          <p>Manage your accounts and banking services</p>
          
          <div className="action-buttons">
            <button 
              className="btn btn-primary btn-large"
              onClick={handleOpenAddAccount}
            >
              Add New Account
            </button>
            <button className="btn btn-secondary btn-large">
              View Accounts
            </button>
            <button className="btn btn-secondary btn-large">
              Account Details
            </button>
          </div>
        </div>

        {showAddAccount && (
          <AddAccount
            customerId={customerId}
            onClose={handleCloseAddAccount}
          />
        )}
      </main>

      <footer className="app-footer">
        <p>&copy; 2024 Test Bank. All rights reserved.</p>
      </footer>
    </div>
  );
}

export default App;
