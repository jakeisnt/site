function print(...txt) {
    return console.log(txt);
};

function runOnDesktop(fn) {
    if (window.matchMedia("(min-width: 501px)")) {
        return fn();
    }
}

function dynLoad(src, id) {
    var s = document.createElement('script');
    s.setAttribute('src', src);
    s.setAttribute('id', id);
    s.setAttribute('async', true);
    return s;
};
