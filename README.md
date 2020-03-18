#Idea

On almost every computer are lots of files that the user doesn't need regularly, but when he needs it, the whole file tree has to be searched because it is not clear where this document could have been saved. Also saving one of these files is really annoying because you have to think of a place to put them.
With the tagger, instead of saving a file to a specific path, you give it some tags and pass it to the program.
Opening a file works by calling the program with some tags describing the document you are looking for.
You can then select one or more of the documents that are provided by the tagger. Then there will be a symbolic link to your cwd, so you can work with and change those files. Once you're done, you can simply delete the symbolic link or change the tags of your files.