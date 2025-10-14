function delay(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

export async function getAccountSubTypes(accountType) {
  await delay(300)
  switch (accountType) {
    case 'Savings Account':
      return ['Regular Savings', 'Salary', 'Senior Citizen']
    case 'Loan Account':
      return ['Home Loan', 'Car Loan', 'Personal Loan']
    case 'Demat Account':
      return ['Individual', 'Joint']
    default:
      return []
  }
}

export async function addCustomerAccount(payload) {
  await delay(500)
  return `Account (${payload.accntType} - ${payload.accntSubType}) created for Customer ${payload.customerId}`
}
