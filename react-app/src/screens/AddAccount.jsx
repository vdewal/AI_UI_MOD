import { useEffect, useMemo, useState } from 'react'
import { getAccountSubTypes, addCustomerAccount } from '../api/mockBankApi.js'

const ACCOUNT_TYPES = ['Savings Account', 'Loan Account', 'Demat Account']
const LOAN_DURATION_OPTIONS = ['1', '2', '3', '4', '5', '6', '7', '8']

function isNonEmptyString(value) {
  return value != null && String(value).trim() !== ''
}

export default function AddAccount({ customerId }) {
  const [accountType, setAccountType] = useState('')
  const [accountSubType, setAccountSubType] = useState('')
  const [ifscCode, setIfscCode] = useState('')
  const [transferLimit, setTransferLimit] = useState('')
  const [loanDuration, setLoanDuration] = useState('')
  const [totalLoanAmount, setTotalLoanAmount] = useState('')
  const [rateOfInterest, setRateOfInterest] = useState('')

  const [subTypeOptions, setSubTypeOptions] = useState([])
  const [loadingSubTypes, setLoadingSubTypes] = useState(false)
  const [subTypeError, setSubTypeError] = useState(null)
  const [submitting, setSubmitting] = useState(false)

  const visibility = useMemo(() => ({
    showIFSC: accountType === 'Savings Account' || accountType === 'Loan Account',
    showTransferLimit: accountType === 'Savings Account',
    showLoanFields: accountType === 'Loan Account',
  }), [accountType])

  useEffect(() => {
    if (!isNonEmptyString(accountType)) {
      setSubTypeOptions([])
      setAccountSubType('')
      return
    }

    let cancelled = false
    setLoadingSubTypes(true)
    setSubTypeError(null)

    getAccountSubTypes(accountType)
      .then((list) => {
        if (!cancelled) {
          setSubTypeOptions(list)
          setAccountSubType('')
        }
      })
      .catch(() => {
        if (!cancelled) {
          setSubTypeError('Failed to load account sub types')
          setSubTypeOptions([])
        }
      })
      .finally(() => {
        if (!cancelled) setLoadingSubTypes(false)
      })

    return () => { cancelled = true }
  }, [accountType])

  const canCreate = useMemo(() => {
    const hasSubType = isNonEmptyString(accountSubType)
    const hasType = isNonEmptyString(accountType)
    const hasIFSC = isNonEmptyString(ifscCode)

    const transferLimitNum = parseInt(transferLimit, 10) || 0
    const roiNum = parseFloat(rateOfInterest) || 0
    const totalLoanNum = parseInt(totalLoanAmount, 10) || 0
    const hasLoanDuration = isNonEmptyString(loanDuration)

    const savingsValid = hasSubType && hasType && hasIFSC && transferLimitNum !== 0 && accountType === 'Savings Account'
    const loanValid = hasSubType && hasType && hasIFSC && roiNum !== 0 && totalLoanNum !== 0 && hasLoanDuration && accountType === 'Loan Account'

    return savingsValid || loanValid
  }, [accountSubType, accountType, ifscCode, transferLimit, rateOfInterest, totalLoanAmount, loanDuration])

  async function handleCreate(event) {
    event.preventDefault()
    if (!canCreate || submitting) return

    setSubmitting(true)
    try {
      const payload = {
        accntSubType: accountSubType,
        accntType: accountType,
        ifscCode: String(ifscCode).trim(),
        transferLimit: parseInt(transferLimit, 10) || 0,
        rateOfInt: parseFloat(rateOfInterest) || 0,
        totalLoanAmt: parseInt(totalLoanAmount, 10) || 0,
        loanDuration: parseInt(loanDuration, 10) || 0,
        customerId,
      }

      const status = await addCustomerAccount(payload)
      window.alert(status)
    } catch (error) {
      window.alert('Failed to create account')
    } finally {
      setSubmitting(false)
    }
  }

  function handleCancel() {
    setAccountType('')
    setAccountSubType('')
    setIfscCode('')
    setTransferLimit('')
    setLoanDuration('')
    setTotalLoanAmount('')
    setRateOfInterest('')
    setSubTypeOptions([])
  }

  return (
    <form onSubmit={handleCreate} style={{ maxWidth: 720, margin: '0 auto', textAlign: 'left' }}>
      <div className="field">
        <label>Account Type</label>
        <select value={accountType} onChange={(e) => setAccountType(e.target.value)}>
          <option value="">Select...</option>
          {ACCOUNT_TYPES.map((type) => (
            <option key={type} value={type}>{type}</option>
          ))}
        </select>
      </div>

      <div className="field">
        <label>Acc Sub Type</label>
        <select
          value={accountSubType}
          onChange={(e) => setAccountSubType(e.target.value)}
          disabled={!isNonEmptyString(accountType) || loadingSubTypes}
        >
          <option value="">{loadingSubTypes ? 'Loading...' : 'Select...'}</option>
          {subTypeOptions.map((opt) => (
            <option key={opt} value={opt}>{opt}</option>
          ))}
        </select>
        {subTypeError && <div className="error">{subTypeError}</div>}
      </div>

      {visibility.showIFSC && (
        <div className="field">
          <label>IFSC Code</label>
          <input type="text" value={ifscCode} onChange={(e) => setIfscCode(e.target.value)} />
        </div>
      )}

      {visibility.showTransferLimit && (
        <div className="field">
          <label>Transfer Limit</label>
          <input type="number" min="0" step="1" value={transferLimit} onChange={(e) => setTransferLimit(e.target.value)} />
        </div>
      )}

      {visibility.showLoanFields && (
        <>
          <div className="field">
            <label>Loan Duration (yrs)</label>
            <select value={loanDuration} onChange={(e) => setLoanDuration(e.target.value)}>
              <option value="">Select...</option>
              {LOAN_DURATION_OPTIONS.map((d) => (
                <option key={d} value={d}>{d}</option>
              ))}
            </select>
          </div>

          <div className="field">
            <label>Total Loan Amount</label>
            <input type="number" min="0" step="1" value={totalLoanAmount} onChange={(e) => setTotalLoanAmount(e.target.value)} />
          </div>

          <div className="field">
            <label>Rate of Interest (%)</label>
            <input type="number" min="0" step="0.01" value={rateOfInterest} onChange={(e) => setRateOfInterest(e.target.value)} />
          </div>
        </>
      )}

      <div style={{ display: 'flex', gap: 12, marginTop: 16 }}>
        <button type="submit" disabled={!canCreate || submitting}>{submitting ? 'Creating...' : 'Create'}</button>
        <button type="button" onClick={handleCancel}>Cancel</button>
      </div>
    </form>
  )
}
