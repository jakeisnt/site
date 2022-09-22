function dynLoad(src, id) {
    var s = document.createElement('script');
    console.log('i am hypothesis');
    s.setAttribute('src', src);
    s.setAttribute('id', id);
    s.setAttribute('async', true);
    return s;
};
function loadHypothesis() {
    __PS_MV_REG = [];
    return document.body.appendChild(dynLoad('https://hypothes.is/embed.js', 'hypothesis'));
};
function unloadHypothesis() {
    var annotatorLink = document.querySelector('link[type=\"application/annotator+html\"]');
    if (annotatorLink) {
        var destroyEvent = new Event('destroy');
        __PS_MV_REG = [];
        return annotatorLink.dispatchEvent(destroyEvent);
    };
};
loadHypothesis();