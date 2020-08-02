// tailwind.config.js
module.exports = {
  purge: [
    './public/index.js',
  ],
  theme: {
    extend: {
      colors: {
        'tps-black': '#1d222d',
        //'tps-button-enabled-background': '#8490a9',
        'tps-disabled': '#ababab',
        'tps-enabled': '#c4953a',
        'tps-mobile-banner': '#dabf8b',
        // mobile border same as button background

      }
    }
  },
  variants: {},
  plugins: [],
}
