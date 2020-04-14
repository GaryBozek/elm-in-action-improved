# elm-in-action

This repository is for the _**Elm In Action**_ book by _Richard Feldman_.

The source changes are pushed at the end of the chapter and are given a Chapter Tag.

## Creating a Tag (in VSCode):

1. Save changes to the repository until the Version/Release is ready to be tagged
2. Sync your personal repository to GitHub
3. Create the Tag
   - Open the Command Pallette (`<ctrl><shift>p`)
   - Choose: `Git: Create Tag`
   - Enter the Tag Name
   - Enter the Message (it is creating an annotated tag)
4. Push the Tag to the repository
   - Open the Command Pallette (`<ctrl><shift>p`)
   - Choose: `Git: Push (Follow Tags)`


## Finding a Tagged Version (in GitHub):

1. Go to the root of the repository
2. Click the `Branch: master` Drop Down List Box
3. Click the `Tag` Tab  **-OR-**  Enter the Tag in the Search box  **-OR-**  Click on the `release` Link
4. Click on the Tag desired
   - This will open the source code associated with the Version/Release


## Finding a Tagged Version (in VSCode):

1. Make sure all current changes in your local repository are saved
2. Open the Command Pallette (`<ctrl><shift>p`)
   - Choose: `Git: Checkout to..`
   - This will open a list of Branches and Tags available, or allow a new Branch to be created
   - Choose the Branch or Tag

 