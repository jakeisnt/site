function print(...txt) {
    return console.log(txt);
};

function runOnDesktop(fn) {
    if (window.matchMedia("(min-width: 501px)")) {
        return fn();
    }
}

function create(elementName, attributes) {
    const elem = document.createElement(elementName);
    for (let key in attributes) {
        elem.setAttribute(key, attributes[key]);
    }
    return elem;
}

function dynLoad(src, id) {
    var s = document.createElement('script');
    s.setAttribute('src', src);
    s.setAttribute('id', id);
    s.setAttribute('async', true);
    return s;
};
