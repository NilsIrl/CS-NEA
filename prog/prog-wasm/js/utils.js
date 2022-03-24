export function print(content) {
  self.postMessage({ type: "print", inner: content.slice() });
}

export function close() {
  self.postMessage({ type: "close" });
}
