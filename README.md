# FormattableTables
Making tables using the formattable package in R

The scripts associated with this project will help you download, manipulate, and format data from AIMS in a way that you can make tables that are informative and look appealing. 

Before beginning this tutorial it is important to have: 
1. [GitHub](https://github.com/) account
2. [Git](https://git-scm.com/) downloaded 
3. Possess an [AIMS API Key](https://open-aims.github.io/data-platform/key-request) 
4. [R](https://cran.r-project.org/bin/windows/base/) and [Rstudio](https://www.rstudio.com/)

The last two are the most important, but hopefully we can quickly review how to set up RStudio with GitHub (through) Git, for anyone that needs to. This will make it easier in the future as you can directly upload your scripts to GitHub as you work. Instead, of moving it all to GitHub once you are done. Keeping your code on GitHub is also a way to backup your scripts, as GitHub can also work as a cloud storage system for your work. 

If you want the files ahead of time you can download the .zip file from GitHub by clicking on the CODE button and then download ZIP in the drop down menu

If you want you can try to download the files directly from GitHub into R you can try following the steps below: 

Create a new repository at github.com. (this is your repository)
Give it the same name as the other repository (In this case FormattableTables).
DONT initialize it with a README, .gitignore, or license.

Clone the repository to your local machine. 
Make sure your directory in R is already set.
Do this using the Git shell in R
Git tab --> more (gear with drop down arrow) --> shell

git clone https://github.com/schmidte10/FormattableTables.git

Rename the local repository's current 'origin' to 'upstream'.
git remote rename origin upstream

Give the local repository an 'origin' that points to your repository.
git remote add origin https://github.com/YOUR-ACCOUNT/FormattableTables.git

Push the local repository to your repository on github.
git push origin master

Now 'origin' points to your repository & 'upstream' points to my original repository.

Create a new branch for your changes with 
git checkout -b my-feature-branch.

You can git commit as usual to your repository.
Use 
git pull upstream master 
to pull changes from the original repository to your master branch.
