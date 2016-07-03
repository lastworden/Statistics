a = """          "YOB" "GenderMale" "Income" "HouseholdStatusDomestic Partners (w/kids)" "HouseholdStatusMarried (no kids)" "HouseholdStatusMarried (w/kids)" "HouseholdStatusSingle (no kids)" "HouseholdStatusSingle (w/kids)" "EducationLevelBachelor's Degree" "EducationLevelCurrent K-12" "EducationLevelCurrent Undergraduate" "EducationLevelDoctoral Degree" "EducationLevelHigh School Diploma" "EducationLevelMaster's Degree" "Q124742Yes" "Q124122Yes" "Q123464Yes" "Q123621Yes" "Q122769Yes" "Q122770Yes" "Q122771Public" "Q122120Yes" "Q121699Yes" "Q121700Yes" "Q120978Yes" "Q121011Yes" "Q120379Yes" "Q120650Yes" "Q120472Science" "Q120194Try first" "Q120012Yes" "Q120014Yes" "Q119334Yes" "Q119851Yes" "Q119650Receiving" "Q118892Yes" "Q118117Yes" "Q118232Pragmatist" "Q118233Yes" "Q118237Yes" "Q117186Hot headed" "Q117193Standard hours" "Q116797Yes" "Q116881Right" "Q116953Yes" "Q116601Yes" "Q116441Yes" "Q116448Yes" "Q116197P.M." "Q115602Yes" "Q115777Start" "Q115610Yes" "Q115611Yes" "Q115899Me" "Q115390Yes" "Q114961Yes" "Q114748Yes" "Q115195Yes" "Q114517Yes" "Q114386TMI" "Q113992Yes" "Q114152Yes" "Q113583Tunes" "Q113584Technology" "Q113181Yes" "Q112478Yes" "Q112512Yes" "Q112270Yes" "Q111848Yes" "Q111580Supportive" "Q111220Yes" "Q110740PC" "Q109367Yes" "Q108950Risk-friendly" "Q109244Yes" "Q108855Yes!" "Q108617Yes" "Q108856Space" "Q108754Yes" "Q108342Online" "Q108343Yes" "Q107869Yes" "Q107491Yes" "Q106993Yes" "Q106997Yay people!" "Q106272Yes" "Q106388Yes" "Q106389Yes" "Q106042Yes" "Q105840Yes" "Q105655Yes" "Q104996Yes" "Q103293Yes" "Q102906Yes" "Q102674Yes" "Q102687Yes" "Q102289Yes" "Q102089Rent" "Q101162Pessimist" "Q101163Mom" "Q101596Yes" "Q100689Yes" "Q100680Yes" "Q100562Yes" "Q99982Nope" "Q100010Yes" "Q99716Yes" "Q99581Yes" "Q99480Yes" "Q98869Yes" "Q98578Yes" "Q98059Yes" "Q98078Yes" "Q98197Yes" "Q96024Yes" "Party" 


"""

b = [i.replace('"',"").replace("\n","") for i in a.split(" ")]
b = [i for i in b if i]

str = "+".join(["exp(%s)"%i for i in b])
print("\n",str,"\n")
