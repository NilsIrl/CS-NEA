const path = require("path");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin")
const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");
const CopyPlugin = require("copy-webpack-plugin");

module.exports = {
  entry: "./js/index.js",
  experiments: {
    asyncWebAssembly: true,
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: ["style-loader", "css-loader"],
      },
    ]
  },
  plugins: [
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "../prog-wasm"),
    }),
    new MonacoWebpackPlugin({ languages: [] }),
    new CopyPlugin({
      patterns: [
        "public",
      ]
    }),
  ],
};
