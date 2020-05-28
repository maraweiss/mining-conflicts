import scrapy
import csv
import json


class QuotesSpider(scrapy.Spider):
    name = "miningconflicts"
    start_urls = []
    with open('../listOfConflicts.data') as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')
        for line in csv_reader:
            if len(line) > 0:
                for url in line:
                    if len(url) > 0:
                        url = "https://ejatlas.org" + url
                        start_urls.append(url)

    def parse(self, response):
        res = {
            'Title': response.xpath("//div[contains(@id, 'disclaimer')]/h2/text()").getall(),
            'Headline': response.xpath("//p[contains(@class, 'headline')]/em/text()").getall(),
            'Description': "",
            'NameOfConflict': response.xpath("//td[contains(text(), 'Name of conflict')]/following-sibling::td/text()").getall(),
            'Country': "",
            'State': "",
            'Location': response.xpath("//td[contains(text(), 'Location of conflict:')]/following-sibling::td/text()").getall(),
            'AccuracyOfLocation': response.xpath("//td[contains(text(), 'Accuracy of location')]/following-sibling::td/text()").getall(),
            'TypeOfConflictLevel1': response.xpath("//td[contains(text(), 'Type of conflict. 1st level:')]/following-sibling::td/text()").getall(),
            'TypeOfConflictLevel2': response.xpath("//td[contains(text(), 'Type of conflict. 2nd level')]/following-sibling::td/text()").getall(),
            'SpecificCommodities': "",
            'ProjectDetails': "",
            'ProjectArea': response.xpath("//td[contains(text(), 'Project area')]/following-sibling::td/text()").getall(),
            'LevelOfInvestment': response.xpath("//td[contains(text(), 'Level of Investment')]/following-sibling::td/text()").getall(),
            'TypeOfPopulation': response.xpath("//td[contains(text(), 'Type of population')]/following-sibling::td/text()").getall(),
            'AffectedPopulation': response.xpath("//td[contains(text(), 'Affected Population')]/following-sibling::td/text()").getall(),
            'StartOfTheConflict': response.xpath("//td[contains(text(), 'Start of the conflict')]/following-sibling::td/text()").getall(),
            'EndOfTheConflict': response.xpath("//td[contains(text(), 'End of the conflict ')]/following-sibling::td/text()").getall(),
            'CompanyNames': "",
            'RelevantGovernmentActors': response.xpath("//td[contains(text(), 'Relevant government actors')]/following-sibling::td/text()").getall(),
            'InternationalFinanceInstitutions': "",
            'EnvironmentalJusticeOrganizations': response.xpath("//td[contains(text(), 'Environmental justice organizations')]/following-sibling::td/text()").getall(),
            'Intensity': response.xpath("//td[contains(text(), 'Intensity')]/following-sibling::td/text()").getall(),
            'ReactionStage': response.xpath("//td[contains(text(), 'Reaction stage')]/following-sibling::td/text()").getall(),
            'GroupsMobilizing': response.xpath("//td[contains(text(), 'Groups mobilizing')]/following-sibling::td/text()").getall(),
            'FormsOfMobilization': response.xpath("//td[contains(text(), 'Forms of mobilization')]/following-sibling::td/text()").getall(),
            'EnvironmentalImpactsVisible': [response.xpath("//td[contains(text(), 'Environmental Impacts')]/following-sibling::td/strong[contains(text(),'Visible:')]/following-sibling::text()").get()],
            'EnvironmentalImpactsPotential': [response.xpath("//td[contains(text(), 'Environmental Impacts')]/following-sibling::td/strong[contains(text(),'Potential:')]/following-sibling::text()").get()],
            'HealthImpactsVisible': [response.xpath("//td[contains(text(), 'Health Impacts')]/following-sibling::td/strong[contains(text(),'Visible:')]/following-sibling::text()").get()],
            'HealthImpactsPotential': [response.xpath("//td[contains(text(), 'Health Impacts')]/following-sibling::td/strong[contains(text(),'Potential')]/following-sibling::text()").get()],
            'SocioeconomicImpactsVisible': [response.xpath("//td[contains(text(), 'Socio-economical Impacts')]/following-sibling::td/strong[contains(text(),'Visible:')]/following-sibling::text()").get()],
            'SocioeconomicImpactsPotential': [response.xpath("//td[contains(text(), 'Socio-economical Impacts')]/following-sibling::td/strong[contains(text(),'Potential')]/following-sibling::text()").get()],
            'ProjectStatus': response.xpath("//td[contains(text(), 'Project Status')]/following-sibling::td/text()").getall(),
            'ConflictOutcome': response.xpath("//td[contains(text(), 'Conflict outcome')]/following-sibling::td/text()").getall(),
            'WasJusticeServed': response.xpath("//td[contains(text(), 'Do you consider this an environmental justice success?')]/following-sibling::td/text()").getall(),
            'BrieflyExplain': response.xpath("//td[contains(text(), 'Briefly explain')]/following-sibling::td/text()").getall(),
            'LastUpdate': response.xpath("//td[contains(text(), 'Last update')]/following-sibling::td/text()").getall()
        }

        description = response.xpath("//div[contains(text(), 'Description:')]/following-sibling::div[contains(@class, 'content')]//td[contains(@class, 'columns')]/p/text()").getall()
        if len(description) == 0:
            description_less = response.xpath("//div[contains(text(), 'Description:')]/following-sibling::div[contains(@class, 'content')]//div[contains(@class, 'less')]/p/text()").getall()
            description_more = response.xpath("//div[contains(text(), 'Description:')]/following-sibling::div[contains(@class, 'content')]//div[contains(@class, 'more')]/p/text()").getall()
            description = description_less + description_more
        res["Description"] = description

        country = response.xpath("//td[contains(text(), 'Country:')]/following-sibling::td/a/text()").getall()
        if len(country) == 0:
            country = response.xpath("//td[contains(text(), 'Country:')]/following-sibling::td/text()").getall()
        res["Country"] = country

        state = response.xpath("//td[contains(text(), 'State or province:')]/following-sibling::td/a/text()").getall()
        if len(state) == 0:
            state = response.xpath("//td[contains(text(), 'State or province:')]/following-sibling::td/text()").getall()
        res["State"] = state

        commodities_link = response.xpath("//td[contains(text(), 'Specific commodities')]/following-sibling::td/a/text()").getall()
        commodities_nolink = response.xpath("//td[contains(text(), 'Specific commodities')]/following-sibling::td/text()").getall()
        res["SpecificCommodities"] = commodities_link + commodities_nolink

        project_details = response.xpath("//td[contains(text(), 'Project details:')]/following-sibling::td[contains(@class, 'columns')]/p/text()").getall()
        if len(project_details) == 0:
            project_details_less = response.xpath("//td[contains(text(), 'Project details:')]/following-sibling::td[contains(@class, 'columns')]//div[contains(@class, 'less')]/p/text()").getall()
            project_details_more = response.xpath("//td[contains(text(), 'Project details:')]/following-sibling::td[contains(@class, 'columns')]//div[contains(@class, 'more')]/p/text()").getall()
            project_details = project_details_less + project_details_more
        res["ProjectDetails"] = project_details

        company_names = []
        i = 1
        while True:
            company = response.xpath("//td[contains(text(), 'Company names or state enterprises')]/following-sibling::td/a[" + str(i) + "]/text()").getall()
            if len(company) == 0:
                break
            i += 1
            company_country = response.xpath("//td[contains(text(), 'Company names or state enterprises')]/following-sibling::td/a[" + str(i) + "]/small/text()").getall()
            if len(company_country) > 0:
                company[0] += " from " + company_country[0]
                i += 1
            company_names += company
        res["CompanyNames"] = company_names

        finance_names = []
        i = 1
        while True:
            finance = response.xpath("//td[contains(text(), 'International and Finance Institutions')]/following-sibling::td/a[" + str(i) + "]/text()").getall()
            if len(finance) == 0:
                break
            i += 1
            finance_country = response.xpath("//td[contains(text(), 'International and Finance Institutions')]/following-sibling::td/a[" + str(i) + "]/small/text()").getall()
            if len(finance_country) > 0:
                finance[0] += " from " + finance_country[0]
                i += 1
            finance_names += finance
        res["InternationalFinanceInstitutions"] = finance_names

        with open("./ejatlas.json", 'a+') as outfile:
            for key in res:
                res[key] = [str(el).encode('ascii', 'ignore').decode('unicode_escape') for el in res[key]]  # encode
                res[key] = [el.replace("•", "") for el in res[key]]
                res[key] = [el.replace(";", ",") for el in res[key]]
                if key == "EnvironmentalImpactsPotential" or key == "EnvironmentalImpactsVisible" or key == "SocioeconomicImpactsPotential" or key == "SocioeconomicImpactsVisible" or key == "HealthImpactsPotential" or key == "HealthImpactsVisible":
                    res[key] = res[key][0].split(",")
                res[key] = [el.lstrip(' ') for el in res[key]]
            json.dump(res, outfile)
            outfile.write(", ")

        file = open("./ejatlas.csv", "a+")
        writeme = ""
        for j, key in enumerate(res):
            for i, el in enumerate(res[key]):
                writestr = str(el).encode('ascii', 'ignore').decode('unicode_escape')  # encode
                writestr = writestr.replace("•", "")
                writestr = writestr.replace(";", ",")
                writestr = " ".join(writestr.split())
                writeme += "'" + writestr + "'"
                if i < len(res[key])-1:
                    writeme += ", "
            if j < len(res)-1:
                writeme += "; "
            else:
                writeme += "\n"
        file.write(writeme)
