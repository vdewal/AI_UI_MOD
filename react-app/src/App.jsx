import './App.css'
import AddAccount from './screens/AddAccount.jsx'

function App() {
  return (
    <div className="app">
      <h1>Add Account</h1>
      <AddAccount customerId={1001} />
    </div>
  )
}

export default App
