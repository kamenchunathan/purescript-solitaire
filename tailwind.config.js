/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{js,ts,purs}"],
  theme: {
    extend: {
      colors: {
        'charleston-green': '#202523ff',
        'old-lavender': '#6c626fff',
        'xiketic': '#202523ff',
        'tuscany': '#b89a9fff',
        'xanadu': '#728472ff'
      }
    },
  },
  plugins: [],
}
