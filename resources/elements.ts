import { create, all } from "./lib";

declare var hljs: any | undefined;

/* enhancements to native html elements */

/**
 * Improve code blocks with button interactivity
 */
const loadCodeBlocks = () => {
  const codeBlocks = "pre code";

  all(codeBlocks).forEach((codeBlock) => {
    const codeContainer = codeBlock.parentElement;

    if (!codeContainer) {
      console.log(`Couldn't find a code container. Running away!`);
      return;
    }

    codeContainer.style.position = "relative";

    const buttonCodeContainer = create(
      "div",
      { class: "insetButtonPanel" },
      codeContainer
    );

    const btn = create(
      "button",
      {
        class: "inset",
        innerText: "Copy",
        onclick: (e: MouseEvent) => {
          const codeContents = (codeBlock as HTMLPreElement).innerText;
          navigator.clipboard.writeText(codeContents);
        },
      },
      buttonCodeContainer
    );

    const label = create(
      "div",
      {
        class: "label inset",
        innerText: codeBlock.classList[0].replace("language-", ""),
      },
      codeContainer
    );

    console.log(label);

    if (codeBlock.classList[1] === "has-raw-code") {
      const rawCodeURL = codeBlock.classList[2];
      const rawCode = create(
        "button",
        {
          class: "inset",
          innerText: "Raw",
          href: rawCodeURL,
          onclick: (e) => {
            console.log("opening window", rawCodeURL);
            window.open(rawCodeURL, "_blank");
          },
        },
        buttonCodeContainer
      );

      console.log(rawCode);
    }
  });

  all(codeBlocks).forEach((block) => {
    hljs.highlightElement(block);
  });
};

const loadHeadings = () => {
  /* Allow users to copy links from headings */
  const headings = "h1, h2, h3, h4, h5, h6";

  all(headings).forEach((heading) => {
    const headingId = heading.id;
    const btn = create("button", {
      className: "inlineText",
      innerText: "#",
      onclick: (e) => {
        console.log(e.target);
        const url = window.location.origin + window.location.pathname;
        const link = `${url}#${headingId}`;
        navigator.clipboard.writeText(link);
      },
    });

    heading.prepend(btn);
  });
};

/* Support footnotes */
const supportFootnotes = () => {
  const footnoteReferences = "sup";
  const footnotes = 'li[id^="fn-"]';

  all(footnoteReferences).forEach((footnoteRef) => {
    footnoteRef.classList.add("footnoteRef");
    const footnoteId = footnoteRef.id;
    const refNum = footnoteId.replace("fnref-", "");
    (footnoteRef.children[0] as HTMLPreElement).innerText = `[${refNum}]`;
  });

  all(footnotes).forEach((footnote) => {
    footnote.classList.add("footnote");
  });
};

document.addEventListener("DOMContentLoaded", (event) => {
  loadCodeBlocks();
  loadHeadings();
  supportFootnotes();
});
