import { create, all } from "./lib";
/* enhancements to native html elements */
/**
 * Improve code blocks with button interactivity
 */
var loadCodeBlocks = function () {
    var codeBlocks = "pre code";
    all(codeBlocks).forEach(function (codeBlock) {
        var codeContainer = codeBlock.parentElement;
        if (!codeContainer) {
            console.log("Couldn't find a code container. Running away!");
            return;
        }
        codeContainer.style.position = "relative";
        var buttonCodeContainer = create("div", { class: "insetButtonPanel" }, codeContainer);
        var btn = create("button", {
            class: "inset",
            innerText: "Copy",
            onclick: function (e) {
                var codeContents = codeBlock.innerText;
                navigator.clipboard.writeText(codeContents);
            },
        }, buttonCodeContainer);
        var label = create("div", {
            class: "label inset",
            innerText: codeBlock.classList[0].replace("language-", ""),
        }, codeContainer);
        if (codeBlock.classList[1] === "has-raw-code") {
            var rawCodeURL_1 = codeBlock.classList[2];
            var rawCode = create("button", {
                class: "inset",
                innerText: "Raw",
                href: rawCodeURL_1,
                onclick: function (e) {
                    console.log("opening window", rawCodeURL_1);
                    window.open(rawCodeURL_1, "_blank");
                },
            }, buttonCodeContainer);
        }
    });
};
var loadHeadings = function () {
    /* Allow users to copy links from headings */
    var headings = "h1, h2, h3, h4, h5, h6";
    all(headings).forEach(function (heading) {
        var headingId = heading.id;
        var btn = create("button", {
            className: "inlineText",
            innerText: "#",
            onclick: function (e) {
                console.log(e.target);
                var url = window.location.origin + window.location.pathname;
                var link = "".concat(url, "#").concat(headingId);
                navigator.clipboard.writeText(link);
            },
        });
        heading.prepend(btn);
    });
};
/* Support footnotes */
var supportFootnotes = function () {
    var footnoteReferences = "sup";
    var footnotes = 'li[id^="fn-"]';
    all(footnoteReferences).forEach(function (footnoteRef) {
        footnoteRef.classList.add("footnoteRef");
        var footnoteId = footnoteRef.id;
        var refNum = footnoteId.replace("fnref-", "");
        footnoteRef.children[0].innerText = "[".concat(refNum, "]");
    });
    all(footnotes).forEach(function (footnote) {
        footnote.classList.add("footnote");
    });
};
document.addEventListener("DOMContentLoaded", function (event) {
    loadCodeBlocks();
    loadHeadings();
    supportFootnotes();
});
