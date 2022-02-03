self.addEventListener("message", ev => {
    import ("../../prog-wasm/pkg").then(wasm => {
        switch (ev.data.type) {
            case "code":
                wasm.run(ev.data.inner);
                break;
            case "ast":
                wasm.ast(ev.data.inner);
                break;
        }
        self.postMessage({ type: "close" });
    });
});
