const path = require("path");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin")
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');

module.exports = {
  entry: "./js/index.ts",
  experiments: {
    asyncWebAssembly: true,
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: "ts-loader",
        exclude: /node_modules/,
      },
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
  ],
  resolve: {
    extensions: [".ts", "..."],
  },
};
