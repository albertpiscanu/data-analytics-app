# data-analytics-app

  The project consists of a data analytics app which can be used to get insights into a dataset containing semester and exam grades from a real lecture. In this project, I created a Haskell app that will give a few valuable stats for the course.
  
  The dataset consists of course grades from a real lecture (the names have been changed). The course points were divided into: lecture grades, homework points and exam grades. The dataset has the following problems: 
  •	lecture grades are mapped against email addresses, whereas homework grades and exam grades are mapped against names.
  •	lecture grades also contains entries with no email address. These correspond to students which have not provided an email in a valid form.
  •	so, in order to have a unique key for each student, the table email to name student map was created using a form. However, it contains some typos.
  
  Implemented tasks:
  •	Compute everyone's final exam grade, based on a formula
  •	Exam statistics summary
  •	Implement a function which reads a string in CSV-format to a Table and a function which writes a string from a Table
  •	Operations on Tables
  •	Implement a query language which represents a variety of table transformations
  •	Implement an evaluation function which performs a Query on a given table
  •	Graph queries
  •	Similarities graph, using queries

