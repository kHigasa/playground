constraint check_salary check
( case when sex = '2'
    then case when salary <= 20000
  else 1 end 1
)