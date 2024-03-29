import("../../prog-wasm/pkg").then(prog => {
  prog.init();
  self.addEventListener("message", ev => {
    const settings = [
      ev.data.settings.caseSensitive,
      !ev.data.settings.allowSingleQuotes,
      ev.data.settings.allowWrongNext
    ];
    switch (ev.data.type) {
      case "code":
        prog.run(ev.data.inner, ev.data.channel, ...settings);
        break;
      case "ast":
        prog.ast(ev.data.inner, ...settings);
        break;
    }
    self.postMessage({ type: "done" });
  });
  self.postMessage({ type: "ready" });
});
