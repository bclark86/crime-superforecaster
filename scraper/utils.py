import yaml


def load_yaml(yaml_file='config.yml'):

    with open(yaml_file) as f:
        docs = yaml.load(f, Loader=yaml.FullLoader)

    return docs
