# Test Bank Management System

A React-based web application that replicates the functionality of the OpenEdge AddAccount.w file. This application provides a modern, responsive interface for managing bank accounts.

## Features

- **Dynamic Form Fields**: Shows/hides fields based on account type selection
- **Real-time Validation**: Form validation updates as user types
- **Account Types**: Supports Savings, Loan, and Demat accounts
- **Responsive Design**: Works on desktop and mobile devices
- **Modern UI**: Clean, professional interface with smooth animations

## Account Types

### Savings Account
- Account Sub Type (dynamically loaded)
- IFSC Code
- Transfer Limit

### Loan Account
- Account Sub Type (dynamically loaded)
- IFSC Code
- Loan Duration (1-8 years)
- Total Loan Amount
- Rate of Interest

### Demat Account
- Account Sub Type (dynamically loaded)
- IFSC Code
- Transfer Limit

## Getting Started

### Prerequisites
- Node.js (version 14 or higher)
- npm or yarn

### Installation

1. Navigate to the project directory:
   ```bash
   cd react-app
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Start the development server:
   ```bash
   npm run dev
   ```

4. Open your browser and navigate to `http://localhost:5173`

### Building for Production

```bash
npm run build
```

The built files will be in the `dist` directory.

## Project Structure

```
react-app/
├── src/
│   ├── components/
│   │   ├── AddAccount.jsx      # Main account creation component
│   │   └── AddAccount.css      # Component styles
│   ├── App.jsx                 # Main application component
│   ├── App.css                 # Application styles
│   ├── main.jsx                # Application entry point
│   └── index.css               # Global styles
├── public/
│   └── vite.svg
├── package.json
├── vite.config.js
└── README.md
```

## Usage

1. Click "Add New Account" on the main page
2. Select an account type from the dropdown
3. Fill in the required fields (fields will show/hide based on account type)
4. The form validates in real-time
5. Click "Create" when all required fields are filled
6. The application will simulate a server call and show the result

## API Integration

The current implementation uses mock server calls. To integrate with a real backend:

1. Replace the `mockServerCall` function in `AddAccount.jsx`
2. Update the API endpoints to match your backend
3. Handle authentication and error responses appropriately

## Customization

### Styling
- Modify `AddAccount.css` for component-specific styles
- Update `App.css` for application-wide styles
- The design uses CSS Grid and Flexbox for responsive layouts

### Validation
- Update the `validateForm` function to add custom validation rules
- Modify field requirements in the `isFieldVisible` function

### Account Types
- Add new account types in the `accountTypes` array
- Update the `isFieldVisible` function to handle new account types
- Add corresponding sub-types in the mock server response

## Browser Support

- Chrome (latest)
- Firefox (latest)
- Safari (latest)
- Edge (latest)

## License

This project is for educational purposes and demonstrates the conversion of OpenEdge ABL code to React.