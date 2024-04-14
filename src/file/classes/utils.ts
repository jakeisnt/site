import type { PageSettings } from "../../types/site";
import { File, SourceFile } from ".";

/**
 * Convert a file, wrapping that a file in a parent.
 * The parent file dispatches to a cloned child file for info.
 */
const wrapFile = (
  sourceFile: SourceFile, // the ts file
  getText: (source: SourceFile) => string,
  {
    extension,
    addExtension = false,
    mimeType: mimeTypeArgument,
  }: {
    extension: string;
    // append the extension instead of replacing it
    addExtension?: boolean;
    mimeType?: string;
  },
  getDependencies: (
    source: SourceFile,
    settings: PageSettings
  ) => File[] = () => []
) => {
  const wrappingFile = sourceFile.clone();

  wrappingFile.fakeFileOf = sourceFile;
  wrappingFile.path =
    sourceFile.path[addExtension ? "addExtension" : "replaceExtension"](
      extension
    );

  wrappingFile.write = (config: PageSettings) => {
    const { sourceDir, targetDir } = config;

    // Write the wrapping file
    const targetPath = wrappingFile.path.relativeTo(sourceDir, targetDir);
    targetPath.writeString(wrappingFile.text(config));

    // write the javascript file without an extension
    // const noExtensionPath = targetPath.replaceExtension();
    // noExtensionPath.writeString(sourceFile.text);

    // Write the source file
    sourceFile.write(config);

    return wrappingFile;
  };

  wrappingFile.dependencies = (settings: PageSettings) => {
    return getDependencies(sourceFile, settings);
  };

  wrappingFile.text = () => getText(sourceFile);
  wrappingFile.isDirectory = () => sourceFile.isDirectory();

  Object.defineProperty(wrappingFile, "mimeType", {
    get() {
      return mimeTypeArgument ?? wrappingFile.path.mimeType;
    },
  });

  Object.defineProperty(wrappingFile, "require", {
    get() {
      return require(sourceFile.path.toString());
    },
  });

  return wrappingFile; // the js file
};

export { wrapFile };
