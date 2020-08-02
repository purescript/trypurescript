const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const CopyPlugin = require('copy-webpack-plugin');
const webpack = require('webpack');

module.exports = {
  entry: {
    "index.js": [
     "./public/tailwind.css",
     "./public/ace.css",
     "./public/index.js",
    ],
  },
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      title: 'Try PureScript!',
      template: './public/index.html',
    }),
    new CopyPlugin({
      patterns: [
        {from: 'public/frame-load.js'},
        {from: 'public/img', to: 'img'},
        {from: 'public/CNAME'},
      ],
    }),
  ],
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          'style-loader',
          'css-loader',
        ],
      },
    ],
  },
  output: {
    filename: '[name]',
    path: path.resolve(__dirname, 'dist'),
    publicPath: '/',
  },
};
