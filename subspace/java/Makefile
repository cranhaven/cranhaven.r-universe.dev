all: ClusteringApplier.class JavaObjectFromDataConverter.class 
	jar -cf ../inst/java/added.jar *.class
run: all
	java -cp ../inst/java/i9-weka.jar:../inst/java/i9-subspace.jar:../inst/java/weka.jar:.  ClusteringApplier
JavaObjectFromDataConverter.class: JavaObjectFromDataConverter.java
	javac -cp ../inst/java/i9-subspace.jar:../inst/java/weka.jar JavaObjectFromDataConverter.java;javac -version
ClusteringApplier.class: JavaObjectFromDataConverter.class ClusteringApplier.java
	javac -cp ../inst/java/i9-weka.jar:../inst/java/i9-subspace.jar:../inst/java/weka.jar:. ClusteringApplier.java
clean:
	rm *.class
