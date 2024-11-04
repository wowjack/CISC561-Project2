CSCI-561 --- Project 2
======================

The purpose of this project is to learn about Boolean Satisfiability
(SAT) and the application of SAT to planning.

INSTRUCTIONS
============

Setup
-----

1. Form teams of 2-4 students.  You may create a corresponding github
   team in the course organization.

2. From the github.com interface for this repo, click `Use this
   template` to import the starter code.  Create a new, private repo
   for your team in the course organization.  Name your repo
   `TYY-project-2-TEAMNAME`, where,
   - `T` is one charcter for the term: `f` Fall or `s` for Spring,
   - `YY` is the two digit year, e.g. `19` for 2019,
   - `TEAMNAME` is a name for your team.

3. Clone the repo.

        git clone git@github.com:mines-csci561/TYY-project-2-TEAMNAME

5. Add all team members' names and CWIDs to the `AUTHORS` file.

6. Change the remote for your cloned repo to your newly created team
   repo and push.

7. Double-check that (1) your team's github repo is correctly named
   and (2) only your teammates (and the instructor/TA) can access your
   team's github repo.

8. Complete the function stubs in project-2.lisp.

9. Answer the questions in project-2-report.tex (Use `pdflatex` to
   compile the PDF).

10. Push your code changes to your group repo in the course
    organization before the project deadline.  At the project
    deadline, the TA/instructor will download for grading all
    repositories in the course organization beginning with the prefix
    `TYY-project-2-` as described above

11. Submit your report as a PDF on canvas. Include all group members'
    names in the submitted PDF (only one group member needs to
    submit).

Code
----

The code portion of the project is divided into two parts, each
containing two sub parts.

- Part 0: Conversion to conjunctive normal form:
  - Part 0.a: Convert an arbitrary Boolean expression to negation
    normal form.  Complete the function EXP->NNF.  Expected length of
    the entire function: about 40 lines.
  - Part 0.b: Convert a Boolean expression in negation normal form to
    conjunctive normal form.  Complete the functions %DIST-OR-AND-1
    and %DIST-OR-AND-AND.  Expected total function length: about 10
    lines each

- Part 1: DPLL
  - Part 1.a: Unit propagation.  Complete the function
    DPLL-UNIT-PROPAGATE.  Expected total function length: about 15
    lines.
  - Part 1.b: DPLL.  Complete the function DPLL. Expected total
    function length: about 20 lines.

Report
------

1. Answer the report questions in project-2-report.tex.

2. Submit your report as a PDF on canvas. Include all group members'
   names in the submitted PDF (only one group member needs to submit).


Grading
=======

The grading script will download your repository from the github
course organization at the deadline.  Your repository must (1) be
located in the course organization and (2) have the correct prefix for
the grading script to find it.

Your code will be run for grading using (approximately) the following
procedure:

    sbcl --load project-2.lisp --load testcases.lisp

Your code needs to produce the correct result to receive credit for
each test case. Make sure your code loads cleanly without compilation
errors, extra I/O, or other side effects.

Please consider and appropriately handle potential edge cases in the
input.
