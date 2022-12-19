import mip

def parse(input):
    split = input.split(" ")
    return {
        "id": int(split[1][:-1]),
        "ore": { "ore": int(split[6]) },
        "clay": { "ore": int(split[12]) },
        "obsidian": { "ore": int(split[18]), "clay": int(split[21]) },
        "geode": { "ore": int(split[27]), "obsidian": int(split[30]) },
    }


def optimize(blueprint, time):
    model = mip.Model(sense=mip.MAXIMIZE)
    objective = mip.LinExpr(const=0)
    variables = {}

    for t in range(time):
        oreBuild = variables[f"ore_{t}"] = model.add_var(f"ore_{t}", var_type=mip.BINARY)
        clayBuild = variables[f"clay_{t}"] = model.add_var(f"clay_{t}", var_type=mip.BINARY)
        obsidianBuild = variables[f"obsidian_{t}"] = model.add_var(f"obsidian_{t}", var_type=mip.BINARY)
        geodeBuild = variables[f"geode_{t}"] = model.add_var(f"geode_{t}", var_type=mip.BINARY)

        model.add_constr(oreBuild + clayBuild + obsidianBuild + geodeBuild <= 1)

        oreBank = mip.LinExpr(const=t)
        clayBank = mip.LinExpr(const=0)
        obsidianBank = mip.LinExpr(const=0)
        for u in range(t):
            oreBank.add_var(variables[f"ore_{u}"], t - u - 1)
            oreBank.add_var(variables[f"ore_{u}"], -blueprint["ore"]["ore"])
            oreBank.add_var(variables[f"clay_{u}"], -blueprint["clay"]["ore"])
            oreBank.add_var(variables[f"obsidian_{u}"], -blueprint["obsidian"]["ore"])
            oreBank.add_var(variables[f"geode_{u}"], -blueprint["geode"]["ore"])

            clayBank.add_var(variables[f"clay_{u}"], t - u - 1)
            clayBank.add_var(variables[f"obsidian_{u}"], -blueprint["obsidian"]["clay"])

            obsidianBank.add_var(variables[f"obsidian_{u}"], t - u - 1)
            obsidianBank.add_var(variables[f"geode_{u}"], -blueprint["geode"]["obsidian"])

        model.add_constr(blueprint["ore"]["ore"] * oreBuild + blueprint["clay"]["ore"] * clayBuild + blueprint["obsidian"]["ore"] * obsidianBuild + blueprint["geode"]["ore"] * geodeBuild <= oreBank, name=f"oreBank_{t}")
        model.add_constr(blueprint["obsidian"]["clay"] * obsidianBuild <= clayBank, name=f"clayBank_{t}")
        model.add_constr(blueprint["geode"]["obsidian"] * geodeBuild <= obsidianBank, name=f"obsidianBank_{t}")

        objective.add_var(geodeBuild, time - t - 1)

    model.objective = objective
    model.optimize()
    return model.objective_value

with open("../../../../../../main/resources/year2022/day19.txt") as file:
    lines = file.readlines()

score = []
for l in lines:
    blueprint = parse(l)
    score.append(optimize(blueprint, 24) * blueprint["id"])

geodes = []
for l in lines[:3]:
    blueprint = parse(l)
    geodes.append(optimize(blueprint, 32))

print(f"part 1 = {sum(score)}")
print(f"part 2 = {geodes[0] * geodes[1] * geodes[2]}")
