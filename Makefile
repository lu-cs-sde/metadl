clean: FORCE
	./gradlew clean

jar: FORCE
	./gradlew clean
	./gradlew jar

test: FORCE
	./gradlew test

eval: FORCE
	java -jar compiler.jar eval::bottomupnaive -OUT ./out -FACTS ./facts $(EVAL)

souffle: FORCE
	java -jar compiler.jar eval::souffle -OUT ./out -FACTS ./facts $(EVAL)

profilesouffle: FORCE
	java -jar compiler.jar eval::souffle -OUT ./out -FACTS ./facts $(EVAL)

prettyinternal: FORCE
	java -jar compiler.jar pretty::bottomupnaive -OUT ./out -FACTS ./facts $(EVAL)

prettysouffle: FORCE
	java -jar compiler.jar pretty::souffle -OUT ./out -FACTS ./facts $(EVAL)

preprint: FORCE
	java -jar compiler.jar preprint -OUT ./out -FACTS ./facts $(EVAL)

typeprog: FORCE
	java -jar compiler.jar typeprog $(EVAL)

semanticcheck: FORCE
	java -jar compiler.jar semanticcheck $(EVAL)

typecheck: FORCE
	java -jar compiler.jar typecheck $(EVAL)

minidoop: jar
	mkdir -p ./out/minidoop
	mkdir -p ./out/minidoop-mdl
	java -jar compiler.jar eval::souffle -OUT ./out/minidoop-mdl -FACTS $(FACTS) -SEP \\t ./examples/minidoop/minidoop.mdl
	souffle -j8 ./examples/minidoop/minidoop.dl -F$(FACTS) -D./out/minidoop/

FORCE: ;
