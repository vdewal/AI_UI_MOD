// Simple in-memory mock API that simulates the ABL workflows described in the pseudocode.
// This keeps state in-module for demo purposes.

const delay = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const mockDb = {
  customers: new Map([
    [1, {
      CustId: 1,
      FirstName: "Alice",
      LastName: "Anderson",
      Address1: "123 Main St",
      Address2: "Apt 4B",
      City: "Metropolis",
      State: "CA",
      Country: "USA",
      EmailId: "alice@example.com",
      Phone: "555-0100",
      Mobile: "555-9999",
      DateOfBirth: "1990-01-01",
      MaritalStatus: "Single",
      ZIPCode: "90210",
    }],
    [2, {
      CustId: 2,
      FirstName: "Bob",
      LastName: "Brown",
      Address1: "987 Elm St",
      Address2: "",
      City: "Gotham",
      State: "NY",
      Country: "USA",
      EmailId: "bob@example.com",
      Phone: "555-0200",
      Mobile: "555-8888",
      DateOfBirth: "1985-10-21",
      MaritalStatus: "Married",
      ZIPCode: "10001",
    }],
  ]),
  accounts: new Map([
    [1, [
      { SelectRow: false, CustId: 1, AcctNum: 10001, AccountTypeID: 1, AccountType: "Savings Account" },
      { SelectRow: false, CustId: 1, AcctNum: 20001, AccountTypeID: 2, AccountType: "Loan Account" },
    ]],
    [2, [
      { SelectRow: false, CustId: 2, AcctNum: 10002, AccountTypeID: 1, AccountType: "Savings Account" },
    ]],
  ]),
  nextAcctNum: 30000,
  nextCustId: 3,
};

export async function getCustomerDetailsByCustId(custId) {
  await delay(200);
  const data = mockDb.customers.get(custId);
  return data ? { ...data } : null;
}

export async function getCustomerAccountDetailsByCustId(custId) {
  await delay(200);
  const rows = mockDb.accounts.get(custId) || [];
  return rows.map((r) => ({ ...r }));
}

export async function deleteCustomerDetails(custId) {
  await delay(200);
  const exists = mockDb.customers.has(custId);
  mockDb.customers.delete(custId);
  mockDb.accounts.delete(custId);
  return exists ? "Customer deleted." : "Customer not found.";
}

export async function getSavAcctDetails(acctNum) {
  await delay(150);
  return {
    AcctNum: acctNum,
    AccntType: "Savings",
    AccntSubType: "Standard",
    TransferLimit: 5000,
    Balance: 1234.56,
    BranchCode: "BR-01",
    RateOfInterest: 2.3,
    LoanDuration: null,
    TotalLoanAmount: null,
  };
}

export async function getLoanAcctDetails(acctNum) {
  await delay(150);
  return {
    AcctNum: acctNum,
    AccntType: "Loan",
    AccntSubType: "Personal",
    TransferLimit: 0,
    Balance: -8500.0,
    BranchCode: "BR-02",
    RateOfInterest: 7.5,
    LoanDuration: 36,
    TotalLoanAmount: 10000,
  };
}

export async function addOrUpdateCustomer(ttCustomerLike, custId) {
  await delay(200);
  if (custId && mockDb.customers.has(custId)) {
    const merged = { ...mockDb.customers.get(custId), ...ttCustomerLike, CustId: custId };
    mockDb.customers.set(custId, merged);
    return { ...merged };
  }
  const newId = mockDb.nextCustId++;
  const created = { ...ttCustomerLike, CustId: newId };
  mockDb.customers.set(newId, created);
  if (!mockDb.accounts.has(newId)) mockDb.accounts.set(newId, []);
  return { ...created };
}

export async function addAccount(custId, accountType) {
  await delay(150);
  const newAcctNum = mockDb.nextAcctNum++;
  const account = {
    SelectRow: false,
    CustId: custId,
    AcctNum: newAcctNum,
    AccountTypeID: accountType === "Savings Account" ? 1 : 2,
    AccountType: accountType,
  };
  const list = mockDb.accounts.get(custId) || [];
  list.push(account);
  mockDb.accounts.set(custId, list);
  return { ...account };
}

export async function deleteAccount(custId, acctNum) {
  await delay(120);
  const list = mockDb.accounts.get(custId) || [];
  const filtered = list.filter((a) => a.AcctNum !== acctNum);
  mockDb.accounts.set(custId, filtered);
  return true;
}

export async function updateAccountDetails(updatedDetails) {
  await delay(150);
  // No-op in mock
  return { ...updatedDetails };
}

export async function advanceSearch(query) {
  await delay(200);
  // For demo, return the first matching or 0 if none
  const q = String(query || "").toLowerCase();
  for (const [, c] of mockDb.customers) {
    const name = `${c.FirstName} ${c.LastName}`.toLowerCase();
    if (name.includes(q)) return c.CustId;
  }
  return 0;
}
