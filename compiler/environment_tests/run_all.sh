#!/bin/bash
name=__TEST_LIST.sh
echo "Environment Tests"
echo "###############################"
echo "Generates a list of script commands based on "
echo "the .sht files in this directory and runs all" 
echo "###############################"
echo "###############################"
rm -f *.proc.sht
rm -f  $name
touch  $name
chmod 777 $name

echo -e "#!/bin/bash\n" >> $name

ls | grep .sht | awk '{print "./test_env.sh " $0}' | sed "s/.sht//" >> $name
echo "Generated script file"
cat $name
echo "###############################"

echo "Running script file "
echo "###############################"
echo "###############################"
./$name
rm -f $name
rm -f *.proc.sht
echo "All tests and cleanup completed" 
echo "###############################"
echo "###############################"
