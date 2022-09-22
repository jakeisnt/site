function dynLoad(src, id) {
    var s = document.createElement('script');
    s.setAttribute('src', src);
    s.setAttribute('id', id);
    return s;
};
function loadHypothesis() {
    __PS_MV_REG = [];
    return dynLoad('https://hypothes.is/embed.js', 'hypothesis');
};
function unloadHypothesis() {
    var annotatorLink = document.querySelector('link[type=\"application/annotator+html\"]');
    if (annotatorLink) {
        var destroyEvent = new Event('destroy');
        __PS_MV_REG = [];
        return annotatorLink.dispatchEvent(destroyEvent);
    };
};
function greetingCallback() {
    __PS_MV_REG = [];
    return alert('Hello World');
};
loadHypothesis();
greetingCallback();