
begin



      f = addfile(file_name,"r")

      array = f->$variable_name$
      time  = f->time

      number_of_missing_time_slots = sum(time( 1 : (dimsizes(time)-1) ) -  time( 0 : (dimsizes(time)-2 ) ) - 1)

      number_of_missing_values = num(ismissing(array))



end
