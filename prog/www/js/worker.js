import("../../prog-wasm/pkg").then(prog => {
  prog.init();
  self.addEventListener("message", ev => {
    switch (ev.data.type) {
      case "code":
        prog.run(ev.data.inner);
        break;
      case "ast":
        prog.ast(ev.data.inner);
        break;
    }
  });
});
