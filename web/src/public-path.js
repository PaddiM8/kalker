// https://github.com/webpack/webpack/issues/7968#issuecomment-421058484
const url = new URL(document.currentScript.src);
const widgetLink = url.href.substring(0, url.href.lastIndexOf('/') + 1);
if (!__webpack_public_path__) __webpack_public_path__ = widgetLink;