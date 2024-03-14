import re

def pull_all_comments(code_file):
  """Pulls all comments from a code file.

  Args:
    code_file: The path to the code file.

  Returns:
    A list of all comments in the code file.
  """

  comments = []
  with open(code_file, "r") as f:
    for line in f:
      # Strip whitespace from the line.
      line = line.strip()
      # If the line starts with a comment character, add it to the list of comments.
      if line.startswith('"'):
        comments.append(line)

  return comments

a = pull_all_comments("instructions.rs")
print(a)

with open("dic.txt", "w") as file:
  for word in a:
    file.write(word + "\n")