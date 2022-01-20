const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin")

module.exports = {
  mode: "development",
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
    ]
  },
  plugins: [
    new WasmPackPlugin({
      crateDirectory: __dirname,
    })
  ],
  resolve: {
    extensions: [".ts", "..."],
  },
};
