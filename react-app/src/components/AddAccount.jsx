import React, { useState, useEffect } from 'react';
import './AddAccount.css';

const AddAccount = ({ customerId, onClose }) => {
  // State for form fields
  const [formData, setFormData] = useState({
    accountType: '',
    accountSubType: '',
    ifscCode: '',
    transferLimit: '',
    loanDuration: '',
    totalLoanAmount: '',
    rateOfInterest: ''
  });

  // State for UI control
  const [isCreateEnabled, setIsCreateEnabled] = useState(false);
  const [accountSubTypes, setAccountSubTypes] = useState([]);
  const [isLoading, setIsLoading] = useState(false);
  const [message, setMessage] = useState('');

  // Account type options
  const accountTypes = [
    'Savings Account',
    'Loan Account', 
    'Demat Account'
  ];

  // Loan duration options
  const loanDurations = ['1', '2', '3', '4', '5', '6', '7', '8'];

  // Handle account type change
  const handleAccountTypeChange = async (accountType) => {
    setFormData(prev => ({
      ...prev,
      accountType,
      accountSubType: '', // Reset sub type when account type changes
      ifscCode: '',
      transferLimit: '',
      loanDuration: '',
      totalLoanAmount: '',
      rateOfInterest: ''
    }));

    // Show/hide fields based on account type
    if (accountType === 'Savings Account') {
      // Show IFSC and Transfer Limit, hide others
    } else if (accountType === 'Loan Account') {
      // Show IFSC, Loan Duration, Rate of Interest, Total Loan Amount, hide Transfer Limit
    } else if (accountType === 'Demat Account') {
      // Show IFSC and Transfer Limit, hide others
    }

    // Fetch account sub types from server
    if (accountType) {
      await fetchAccountSubTypes(accountType);
    }
  };

  // Fetch account sub types from server
  const fetchAccountSubTypes = async (accountType) => {
    setIsLoading(true);
    try {
      // Simulate server call - replace with actual API call
      const response = await mockServerCall('getAcctSubType', {
        accountType,
        customerId: 0
      });
      
      setAccountSubTypes(response.data || []);
      setMessage(`Account Sub Types: ${JSON.stringify(response)}`);
    } catch (error) {
      setMessage(`Error fetching sub types: ${error.message}`);
    } finally {
      setIsLoading(false);
    }
  };

  // Handle field changes
  const handleFieldChange = (field, value) => {
    setFormData(prev => ({
      ...prev,
      [field]: value
    }));
  };

  // Validation function
  const validateForm = () => {
    const { accountType, accountSubType, ifscCode, transferLimit, loanDuration, totalLoanAmount, rateOfInterest } = formData;

    if (accountType === 'Savings Account' || accountType === 'Demat Account') {
      return accountSubType && accountType && ifscCode && transferLimit && transferLimit > 0;
    } else if (accountType === 'Loan Account') {
      return accountSubType && accountType && ifscCode && 
             rateOfInterest && rateOfInterest > 0 && 
             totalLoanAmount && totalLoanAmount > 0 && 
             loanDuration;
    }
    return false;
  };

  // Update validation on form data change
  useEffect(() => {
    setIsCreateEnabled(validateForm());
  }, [formData]);

  // Handle create account
  const handleCreateAccount = async () => {
    if (!validateForm()) return;

    setIsLoading(true);
    try {
      // Prepare account details
      const accountDetails = {
        accountType: formData.accountType,
        accountSubType: formData.accountSubType,
        ifscCode: formData.ifscCode,
        transferLimit: parseInt(formData.transferLimit) || 0,
        rateOfInterest: parseFloat(formData.rateOfInterest) || 0,
        totalLoanAmount: parseInt(formData.totalLoanAmount) || 0,
        loanDuration: parseInt(formData.loanDuration) || 0
      };

      // Simulate server call - replace with actual API call
      const response = await mockServerCall('AddCustomerAccount', {
        accountType: formData.accountType,
        customerId,
        accountDetails
      });

      setMessage(`Account created successfully: ${JSON.stringify(response)}`);
      
      // Reset form
      setFormData({
        accountType: '',
        accountSubType: '',
        ifscCode: '',
        transferLimit: '',
        loanDuration: '',
        totalLoanAmount: '',
        rateOfInterest: ''
      });
      setAccountSubTypes([]);

    } catch (error) {
      setMessage(`Error creating account: ${error.message}`);
    } finally {
      setIsLoading(false);
    }
  };

  // Mock server call function - replace with actual API calls
  const mockServerCall = async (method, data) => {
    // Simulate network delay
    await new Promise(resolve => setTimeout(resolve, 1000));
    
    // Mock response based on method
    if (method === 'getAcctSubType') {
      const subTypes = {
        'Savings Account': ['Regular Savings', 'Premium Savings', 'Basic Savings'],
        'Loan Account': ['Personal Loan', 'Home Loan', 'Car Loan', 'Education Loan'],
        'Demat Account': ['Regular Demat', 'Premium Demat']
      };
      return { data: subTypes[data.accountType] || [] };
    } else if (method === 'AddCustomerAccount') {
      return { success: true, accountId: Math.floor(Math.random() * 10000) };
    }
    
    return { success: true };
  };

  // Check if field should be visible
  const isFieldVisible = (field) => {
    const { accountType } = formData;
    
    switch (field) {
      case 'ifscCode':
        return true; // Always visible
      case 'transferLimit':
        return accountType === 'Savings Account' || accountType === 'Demat Account';
      case 'loanDuration':
      case 'totalLoanAmount':
      case 'rateOfInterest':
        return accountType === 'Loan Account';
      default:
        return true;
    }
  };

  return (
    <div className="add-account-dialog">
      <div className="dialog-header">
        <h2>Add Account</h2>
        <button className="close-btn" onClick={onClose}>×</button>
      </div>
      
      <div className="dialog-content">
        <div className="form-section">
          <h3>ADD NEW ACCOUNT</h3>
          
          <div className="form-row">
            <div className="form-group">
              <label>Account Type</label>
              <select
                value={formData.accountType}
                onChange={(e) => handleAccountTypeChange(e.target.value)}
                className="form-control"
              >
                <option value="">Select Account Type</option>
                {accountTypes.map(type => (
                  <option key={type} value={type}>{type}</option>
                ))}
              </select>
            </div>
            
            <div className="form-group">
              <label>Account Sub Type</label>
              <select
                value={formData.accountSubType}
                onChange={(e) => handleFieldChange('accountSubType', e.target.value)}
                className="form-control"
                disabled={!formData.accountType}
              >
                <option value="">Select Sub Type</option>
                {accountSubTypes.map(type => (
                  <option key={type} value={type}>{type}</option>
                ))}
              </select>
            </div>
          </div>

          <div className="form-row">
            <div className="form-group">
              <label>IFSC Code</label>
              <input
                type="text"
                value={formData.ifscCode}
                onChange={(e) => handleFieldChange('ifscCode', e.target.value)}
                className="form-control"
                placeholder="Enter IFSC Code"
              />
            </div>
            
            {isFieldVisible('transferLimit') && (
              <div className="form-group">
                <label>Transfer Limit</label>
                <input
                  type="number"
                  value={formData.transferLimit}
                  onChange={(e) => handleFieldChange('transferLimit', e.target.value)}
                  className="form-control"
                  placeholder="Enter Transfer Limit"
                />
              </div>
            )}
            
            {isFieldVisible('loanDuration') && (
              <div className="form-group">
                <label>Loan Duration (yrs)</label>
                <select
                  value={formData.loanDuration}
                  onChange={(e) => handleFieldChange('loanDuration', e.target.value)}
                  className="form-control"
                >
                  <option value="">Select Duration</option>
                  {loanDurations.map(duration => (
                    <option key={duration} value={duration}>{duration}</option>
                  ))}
                </select>
              </div>
            )}
          </div>

          <div className="form-row">
            {isFieldVisible('totalLoanAmount') && (
              <div className="form-group">
                <label>Total Loan Amount</label>
                <input
                  type="number"
                  value={formData.totalLoanAmount}
                  onChange={(e) => handleFieldChange('totalLoanAmount', e.target.value)}
                  className="form-control"
                  placeholder="Enter Loan Amount"
                />
              </div>
            )}
            
            {isFieldVisible('rateOfInterest') && (
              <div className="form-group">
                <label>Rate of Interest (%)</label>
                <input
                  type="number"
                  step="0.01"
                  value={formData.rateOfInterest}
                  onChange={(e) => handleFieldChange('rateOfInterest', e.target.value)}
                  className="form-control"
                  placeholder="Enter Rate of Interest"
                />
              </div>
            )}
          </div>
        </div>

        {message && (
          <div className="message-box">
            <pre>{message}</pre>
          </div>
        )}

        <div className="dialog-actions">
          <button
            className={`btn btn-primary ${isLoading ? 'loading' : ''}`}
            onClick={handleCreateAccount}
            disabled={!isCreateEnabled || isLoading}
          >
            {isLoading ? 'Creating...' : 'Create'}
          </button>
          <button
            className="btn btn-secondary"
            onClick={onClose}
            disabled={isLoading}
          >
            Cancel
          </button>
        </div>
      </div>
    </div>
  );
};

export default AddAccount;
