Rewrite of TryPureScript using latest features of PS ecosystem, such as:
* Halogen Hooks
* Tailwind CSS

Lots of HTML and JS code was eliminated.

Also enables gist saving and tracking state in URL rather than local storage.

### Local Development
```
npm i
npm config set tps:configpath "config/dev/*.purs"
npm run gen-css # Create initial tailwind css files
npm run start # Launch local dev server with automatic reload/refresh.

# Optional:
npm run build # To manually rebuild if IDE does not do this automatically.
npm run lock-css # To speed up rebuilds if you're not adding new css classes.
```

### Building for production
```
npm config set tps:configpath "config/prod/*.purs"
npm run prod # Create minified production build
```
