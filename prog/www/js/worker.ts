self.addEventListener("message", ev => {
    import ("../../prog-wasm/pkg").then(wasm => {
        console.log(ev);
        wasm.run(ev.data.inner);
        self.postMessage({ type: "close" });
    });
});
