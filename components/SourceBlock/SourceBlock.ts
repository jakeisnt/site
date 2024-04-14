import TextFile from "../../src/file/classes/text";
import { escapeHtml } from "../../src/html/utils";
import treeSitter from "tree-sitter-highlight";

const SourceBlock = ({ file }: { file: TextFile }) => ({
  dependsOn: [
    {
      src: "/components/SourceBlock/source-block.css",
    },
    {
      src: "/components/SourceBlock/load-code-blocks.js",
    },
  ],
  body: (() => {
    // This is a quick way to snag the language;
    // we access the enum member with an uppercase version of the file extension.
    // It's fine.
    // @ts-ignore
    const language = treeSitter.Language?.[file.extension?.toUpperCase()];

    return [
      "pre",
      [
        "code",
        { class: `language-${file.extension} has-raw-code` },
        language
          ? treeSitter.highlight(file.text, language)
          : escapeHtml(file.text),
      ],
    ];
  })(),
});

export default SourceBlock;
