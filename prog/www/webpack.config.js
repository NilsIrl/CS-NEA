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
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "../prog-wasm-channel"),
    }),
    new MonacoWebpackPlugin({ languages: [] }),
    new CopyPlugin({
      patterns: [
        "public",
      ]
    }),
  ],
  devServer: {
    client: {
      overlay: {
        warnings: false,
      },
    },
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp",
    },
  },
};
