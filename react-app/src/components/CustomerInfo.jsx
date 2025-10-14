import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import {
  getCustomerDetailsByCustId,
  getCustomerAccountDetailsByCustId,
  deleteCustomerDetails,
  addOrUpdateCustomer,
  addAccount,
  deleteAccount,
  getSavAcctDetails,
  getLoanAcctDetails,
  updateAccountDetails,
  advanceSearch,
} from "../api/client";
import "./customerInfo.css";

const initialCustomer = {
  CustId: "",
  FirstName: "",
  LastName: "",
  Address1: "",
  Address2: "",
  City: "",
  State: "",
  Country: "",
  EmailId: "",
  Phone: "",
  Mobile: "",
  DateOfBirth: "",
  MaritalStatus: "",
  ZIPCode: "",
};

export default function CustomerInfo() {
  const [form, setForm] = useState(initialCustomer);
  const [accounts, setAccounts] = useState([]);
  const [selectAll, setSelectAll] = useState(false);
  const [busy, setBusy] = useState(false);
  const [message, setMessage] = useState("");

  const hasCustomerLoaded = useMemo(() => !!form.CustId && form.FirstName !== "", [form]);
  const canSearch = useMemo(() => Number(form.CustId) > 0, [form.CustId]);
  const anySelected = useMemo(() => accounts.some((a) => a.SelectRow), [accounts]);

  const handleChange = useCallback((e) => {
    const { name, value } = e.target;
    setForm((f) => ({ ...f, [name]: value }));
  }, []);

  const resetAll = useCallback(() => {
    setForm(initialCustomer);
    setAccounts([]);
    setSelectAll(false);
    setMessage("");
  }, []);

  const refreshAccounts = useCallback(async (custId) => {
    const rows = await getCustomerAccountDetailsByCustId(Number(custId));
    setAccounts(rows.map((r) => ({ ...r, SelectRow: false })));
    setSelectAll(false);
  }, []);

  const onSearch = useCallback(async () => {
    if (!canSearch) return;
    setBusy(true);
    setMessage("");
    try {
      const cust = await getCustomerDetailsByCustId(Number(form.CustId));
      if (!cust) {
        setAccounts([]);
        setMessage("Customer Details Not Found...");
        return;
      }
      setForm({ ...initialCustomer, ...cust, CustId: String(cust.CustId) });
      await refreshAccounts(cust.CustId);
    } catch (e) {
      setMessage("Error fetching customer data.");
    } finally {
      setBusy(false);
    }
  }, [canSearch, form.CustId, refreshAccounts]);

  const onClear = useCallback(() => {
    resetAll();
  }, [resetAll]);

  const onDeleteCustomer = useCallback(async () => {
    if (!hasCustomerLoaded) return;
    setBusy(true);
    try {
      const status = await deleteCustomerDetails(Number(form.CustId));
      setMessage(status);
      resetAll();
    } finally {
      setBusy(false);
    }
  }, [form.CustId, hasCustomerLoaded, resetAll]);

  const toggleRow = useCallback((acctNum) => {
    setAccounts((rows) =>
      rows.map((r) => (r.AcctNum === acctNum ? { ...r, SelectRow: !r.SelectRow } : r))
    );
  }, []);

  const onRowDoubleClick = useCallback(async (row) => {
    const details =
      row.AccountType === "Savings Account"
        ? await getSavAcctDetails(row.AcctNum)
        : await getLoanAcctDetails(row.AcctNum);
    const pretty = JSON.stringify(details, null, 2);
    alert(`Account Details for ${row.AcctNum}:\n\n${pretty}`);
  }, []);

  const onToggleSelectAll = useCallback(() => {
    setSelectAll((s) => {
      const next = !s;
      setAccounts((rows) => rows.map((r) => ({ ...r, SelectRow: next })));
      return next;
    });
  }, []);

  const onDetails = useCallback(async () => {
    for (const row of accounts.filter((r) => r.SelectRow)) {
      const details =
        row.AccountType === "Savings Account"
          ? await getSavAcctDetails(row.AcctNum)
          : await getLoanAcctDetails(row.AcctNum);
      const pretty = JSON.stringify(details, null, 2);
      alert(`Account Details for ${row.AcctNum}:\n\n${pretty}`);
    }
  }, [accounts]);

  const onAddCustomer = useCallback(async () => {
    const payload = { ...form, CustId: undefined };
    const created = await addOrUpdateCustomer(payload);
    setForm({ ...initialCustomer, ...created, CustId: String(created.CustId) });
    await refreshAccounts(created.CustId);
    setMessage("Customer added.");
  }, [form, refreshAccounts]);

  const onUpdateCustomer = useCallback(async () => {
    if (!hasCustomerLoaded) return;
    const payload = { ...form, CustId: Number(form.CustId) };
    const updated = await addOrUpdateCustomer(payload, Number(form.CustId));
    setForm({ ...initialCustomer, ...updated, CustId: String(updated.CustId) });
    setMessage("Customer updated.");
  }, [form, hasCustomerLoaded]);

  const onAddAccount = useCallback(async () => {
    if (!hasCustomerLoaded) return;
    const type = window.prompt("Enter Account Type: 'Savings Account' or 'Loan Account'","Savings Account");
    if (!type) return;
    await addAccount(Number(form.CustId), type);
    await refreshAccounts(form.CustId);
  }, [form.CustId, hasCustomerLoaded, refreshAccounts]);

  const onDeleteAccount = useCallback(async () => {
    const selected = accounts.filter((r) => r.SelectRow);
    for (const row of selected) {
      const ok = window.confirm(`Delete account ${row.AcctNum} (${row.AccountType})?`);
      if (!ok) continue;
      await deleteAccount(Number(form.CustId), row.AcctNum);
    }
    await refreshAccounts(form.CustId);
  }, [accounts, form.CustId, refreshAccounts]);

  const onUpdateAccount = useCallback(async () => {
    const selected = accounts.filter((r) => r.SelectRow);
    for (const row of selected) {
      const base =
        row.AccountType === "Savings Account"
          ? await getSavAcctDetails(row.AcctNum)
          : await getLoanAcctDetails(row.AcctNum);
      const newRate = window.prompt(
        `Update RateOfInterest for ${row.AcctNum} (current ${base.RateOfInterest})`,
        String(base.RateOfInterest)
      );
      if (newRate == null) continue;
      await updateAccountDetails({ ...base, RateOfInterest: Number(newRate) });
    }
    setMessage("Account(s) updated.");
  }, [accounts]);

  const onAdvanceSearch = useCallback(async () => {
    const q = window.prompt("Search by name contains:", "");
    if (q == null) return;
    const id = await advanceSearch(q);
    if (id && id !== 0) {
      setForm((f) => ({ ...f, CustId: String(id) }));
      await onSearch();
    } else {
      setMessage("No matches.");
    }
  }, [onSearch]);

  const onEnterOnTable = useCallback(
    async (e) => {
      if (e.key === "Enter") {
        e.preventDefault();
        await onDetails();
      }
    },
    [onDetails]
  );

  const custIdInputRef = useRef(null);
  useEffect(() => {
    if (custIdInputRef.current) custIdInputRef.current.focus();
  }, []);

  return (
    <div className="ci-wrapper">
      <h2>Customer Info</h2>

      {message && <div className="ci-alert">{message}</div>}

      <div className="ci-form">
        <div className="ci-row">
          <label>CustId</label>
          <input
            ref={custIdInputRef}
            name="CustId"
            value={form.CustId}
            onChange={handleChange}
            onKeyDown={(e) => {
              if (e.key === "Enter" && canSearch) onSearch();
            }}
            placeholder="Enter Customer ID"
          />
          <button disabled={!canSearch || busy} onClick={onSearch} title="Search">
            Search
          </button>
          <button onClick={onAdvanceSearch} disabled={busy} title="Advanced Search">
            Adv Search
          </button>
          <button onClick={onClear} disabled={busy} title="Clear">
            Clear
          </button>
        </div>

        <div className="ci-grid">
          <div className="ci-field">
            <label>First Name</label>
            <input name="FirstName" value={form.FirstName} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>Last Name</label>
            <input name="LastName" value={form.LastName} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>Address1</label>
            <input name="Address1" value={form.Address1} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>Address2</label>
            <input name="Address2" value={form.Address2} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>City</label>
            <input name="City" value={form.City} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>State</label>
            <input name="State" value={form.State} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>Country</label>
            <input name="Country" value={form.Country} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>Email</label>
            <input name="EmailId" value={form.EmailId} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>Phone</label>
            <input name="Phone" value={form.Phone} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>Mobile</label>
            <input name="Mobile" value={form.Mobile} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>DOB</label>
            <input name="DateOfBirth" value={form.DateOfBirth} onChange={handleChange} />
          </div>
          <div className="ci-field">
            <label>Marital Status</label>
            <select name="MaritalStatus" value={form.MaritalStatus} onChange={handleChange}>
              <option value="">--</option>
              <option>Single</option>
              <option>Married</option>
              <option>Divorced</option>
              <option>Widowed</option>
            </select>
          </div>
          <div className="ci-field">
            <label>ZIP</label>
            <input name="ZIPCode" value={form.ZIPCode} onChange={handleChange} />
          </div>
        </div>

        <div className="ci-actions">
          <button onClick={onAddCustomer} disabled={busy} title="Add Customer">
            Add
          </button>
          <button onClick={onUpdateCustomer} disabled={!hasCustomerLoaded || busy} title="Update Customer">
            Update
          </button>
          <button onClick={onDeleteCustomer} disabled={!hasCustomerLoaded || busy} title="Delete Customer">
            Delete
          </button>
          <button onClick={onAddAccount} disabled={!hasCustomerLoaded || busy} title="Add Account">
            Add Account
          </button>
          <button onClick={onDeleteAccount} disabled={!anySelected || busy} title="Delete Account(s)">
            Delete Account
          </button>
          <button onClick={onUpdateAccount} disabled={!anySelected || busy} title="Update Account(s)">
            Update Account
          </button>
          <button onClick={onDetails} disabled={!anySelected} title="Show Details">
            Details
          </button>
          <label className="ci-toggle">
            <input type="checkbox" checked={selectAll} onChange={onToggleSelectAll} /> Select All
          </label>
        </div>
      </div>

      <div className="ci-browse" onKeyDown={onEnterOnTable} tabIndex={0}>
        <table>
          <thead>
            <tr>
              <th style={{ width: 60 }}>Select</th>
              <th>AcctNum</th>
              <th>AccountType</th>
              <th>AccountTypeID</th>
              <th>CustId</th>
            </tr>
          </thead>
          <tbody>
            {accounts.length === 0 && (
              <tr>
                <td colSpan={5} style={{ textAlign: "center", color: "#666" }}>
                  No accounts.
                </td>
              </tr>
            )}
            {accounts.map((row) => (
              <tr key={row.AcctNum} onDoubleClick={() => onRowDoubleClick(row)}>
                <td>
                  <input
                    type="checkbox"
                    checked={row.SelectRow || false}
                    onChange={() => toggleRow(row.AcctNum)}
                    onClick={(e) => e.stopPropagation()}
                  />
                </td>
                <td>{row.AcctNum}</td>
                <td>{row.AccountType}</td>
                <td>{row.AccountTypeID}</td>
                <td>{row.CustId}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}
