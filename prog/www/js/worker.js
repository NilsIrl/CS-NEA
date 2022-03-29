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
        prog.run(ev.data.inner, ...settings);
        break;
      case "ast":
        prog.ast(ev.data.inner, ...settings);
        break;
    }
  });
});
