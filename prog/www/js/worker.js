import("../../prog-wasm/pkg").then(prog => {
  self.addEventListener("message", ev => {
    switch (ev.data.type) {
      case "code":
        prog.run(ev.data.inner);
        break;
      case "ast":
        prog.ast(ev.data.inner);
        break;
    }
    self.postMessage({ type: "close" });
  });
});
