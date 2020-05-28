# Crawler for ejatlas.org

this crawler will fetch most data from the Latin American mining conflicts documented on ejatlas

Steps to get the data (tested in chrome):
* go to https://ejatlas.org/featured/mining-latam
* open the javascript console (ctrl+shift+i), go to the console tab
* copy paste the content of crawler.js in the console and press enter
* wait for approximately 5 minutes for the crawler to fetch all URLs. A file with the urls will download after it finished. It is slow because there is a limit on the request frequency, otherwise the page blocks the request
* copy the downloaded output file listOfConflicts.data into the same directory where this readme.md is: mining-conflicts/ejatlasCrawler/listOfConflicts.data
* go to the top of the scrapy subdirectory (mining-conflicts/ejatlasCrawler/ejatlas) and run "scrapy crawl miningconflicts"
* if running scrapy from the preconfigured files didn't work, try:
  - optional: install python virtualenv with pip3 install virtualenv
  - optional: set up virtualenv with virtualenv venv -p python3
  - optional: activate virtualenv with source venv/bin/activate
  - get scrapy with "pip3 install scrapy" (virtualenv recommended)
  - cd into mining-conflicts/ejatlasCrawler and run "scrapy startproject ejatlas_new", scrapy will create a subdirectory
  - copy the file mining-conflicts/ejatlasCrawler/ejatlas/ejatlas/spiders/ejatlas.py into the scrapy "spiders" subdirectory of your newly created directory
  - run "scrapy crawl miningconflicts" from the top of the new scrapy subdirectory again
* scrapy will create a csv and a json file with the scraped data in mining-conflicts/ejatlasCrawler/ejatlas




Csv fields:
Title, Headline, Description, NameOfConflict, Country, State, Location, AccuracyOfLocation, TypeOfConflictLevel1, TypeOfConflictLevel2, SpecificCommodities, ProjectDetails, ProjectArea, LevelOfInvestment, TypeOfPopulation, AffectedPopulation, StartOfTheConflict, EndOfTheConflict, CompanyNames, RelevantGovernmentActors, InternationalFinanceInstitutions, EnvironmentalJusticeOrganizations, Intensity, ReactionStage, GroupsMobilizing, FormsOfMobilization, EnvironmentalImpactsVisible, EnvironmentalImpactsPotential, HealthImpactsVisible, HealthImpactsPotential, SocioeconomicImpactsVisible, SocioeconomicImpactsPotential, ProjectStatus, ConflictOutcome, WasJusticeServed, BrieflyExplain, LastUpdate
