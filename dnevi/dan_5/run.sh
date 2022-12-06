dan=5
ocamlc -g str.cma dan_$dan.ml -o dan_$dan.exe
./dan_$dan.exe
echo -e "Prva rešitev:\n"
cat "dan_""$dan""_1.out"
echo -e "\nDruga rešitev:\n"
cat "dan_""$dan""_2.out"

