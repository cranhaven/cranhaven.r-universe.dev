import argparse
import csv
import numpy as np
import igraph
from tqdm import tqdm


def sequential_interaction_score(max_path_length, graph, edge_list_reader, num_edges):
    """Computes interaction score sequentially and adds it to graph as attribute.

    Arguments:
    max_path_length -- maximum length of simple paths to include in computation
    graph -- igraph graph object to compute interaction score for
    edge_list_reader -- iterator over the lines of an edge list file containing the edges to compute interaction score for

    No return value
    """
    progress_bar = tqdm(desc="Calculating interaction score", total=num_edges)
    for edge in edge_list_reader:
        sums_of_weight_products = [0] * max_path_length
        num_paths = [0] * max_path_length
        simple_paths = graph.get_all_simple_paths(edge[0], edge[1], cutoff = max_path_length)
        for path in simple_paths:
            path_length = len(path) - 1
            path_weights = []
            for i in range(path_length):
                path_weights.append(graph.es.find(_source=path[i], _target=path[i+1])["weight"])
            sums_of_weight_products[path_length - 1] += np.prod(path_weights)
            num_paths[path_length - 1] += 1
        sums_of_weight_products = np.array(sums_of_weight_products, dtype=float)
        num_paths = np.array(num_paths, dtype=float)
        normalized_sums = np.divide(sums_of_weight_products, num_paths, out=np.zeros_like(num_paths), where=num_paths != 0)
        edge_score = edge[0], edge[1], np.sum(normalized_sums)
        add_score_attribute_to_edge(graph, edge_score, "weight")
        progress_bar.update()
    progress_bar.close()

def add_score_attribute(graph, score, score_name):
    """Adds a new score as edge attribute to graph.

    Arguments:
    graph -- igraph graph object
    score -- list of tuples each containing source, target and score of an edge
    score_name -- name of the attribute

    No return value
    """
    for edge_score in score:
        graph.es.find(_source=edge_score[0], _target=edge_score[1])[score_name] = edge_score[2]

def add_score_attribute_to_edge(graph, edge_score, score_name):
    """Fills in a score edge attribute value for a single edge.

    Arguments:
    graph -- igraph graph object
    edge_score -- Tuple with source, target and score value
    score_name -- name of the attribute

    No return value
    """
    graph.es.find(_source=edge_score[0], _target=edge_score[1])[score_name] = edge_score[2]

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("graph_file", help="file containing the input graph in gml format")
    parser.add_argument("edge_list_file", help="file containing the list of edges to calculate the interaction score for in tsv format (source,target,weight)")
    parser.add_argument("max_path_length", default=3, help="maximum length of simple paths considered for score computation")
    parser.add_argument("--output", default="int_score_graph.gml", help="output file name, graph with score in gml format")
    parser.add_argument("--cluster_address", default=None, help="address of ray cluster, if a cluster is used")
    parser.add_argument("--distributed", action="store_const", const=True, default=False, help="run in parallel with ray")
    args = parser.parse_args()

    graph = igraph.read(args.graph_file, format = "gml")
    with open(args.edge_list_file, "r") as el_file:
        edge_list_reader = csv.reader(el_file, delimiter="\t")
        next(edge_list_reader) # skip header
        if args.distributed:
            import ray

            def distributed_interaction_score(max_path_length, graph, edge_list_reader, cluster_address=None):
                """Computes interaction score in parallel using a ray cluster and adds it to graph as attribute.

                Arguments:
                max_path_length -- maximum length of simple paths to include in computation
                graph -- igraph graph object to compute interaction score for
                edge_list_reader -- iterator over the lines of an edge list file containing the edges to compute interaction score for

                Optional:
                cluster_address -- IP address of existing ray cluster to be used. If None it creates a local cluster (default).

                No return value
                """
                @ray.remote
                def single_edge_interaction_score(max_path_length, graph, edge):
                    """Computes interaction score for a single edge and returns it.

                    Arguments:
                    max_path_length -- maximum length of simple paths to include in computation
                    graph -- igraph graph object to compute interaction score for
                    edge -- single line of edge list file

                    Returns:
                    Tuple with source, target and interaction score
                    """
                    sums_of_weight_products = [0] * max_path_length
                    num_paths = [0] * max_path_length
                    simple_paths = graph.get_all_simple_paths(edge[0], edge[1], cutoff = max_path_length)
                    for path in simple_paths:
                        path_length = len(path) - 1
                        path_weights = []
                        for i in range(path_length):
                            path_weights.append(graph.es.find(_source=path[i], _target=path[i+1])["weight"])
                        sums_of_weight_products[path_length - 1] += np.prod(path_weights)
                        num_paths[path_length - 1] += 1
                    sums_of_weight_products = np.array(sums_of_weight_products, dtype=float)
                    num_paths = np.array(num_paths, dtype=float)
                    normalized_sums = np.divide(sums_of_weight_products, num_paths, out=np.zeros_like(num_paths), where=num_paths != 0)
                    return( edge[0], edge[1], np.sum(normalized_sums) )

                if cluster_address is None:
                    ray.init()
                else:
                    ray.init(address=cluster_address)
                remote_graph = ray.put(graph)
                futures = [single_edge_interaction_score.remote(max_path_length, remote_graph, edge) for edge in edge_list_reader]
                progress_bar = tqdm(desc="Calculating interaction score", total=len(futures))
                while len(futures):
                    [done], futures = ray.wait(futures)
                    add_score_attribute_to_edge(graph, ray.get(done), "weight")
                    progress_bar.update()
                progress_bar.close()
                ray.shutdown()

            distributed_interaction_score(int(args.max_path_length), graph, edge_list_reader, args.cluster_address)
        else:
            num_edges = 0
            with open(args.graph_file, "r") as g_file:
                num_edges = sum(1 for _ in g_file)
            sequential_interaction_score(int(args.max_path_length), graph, edge_list_reader, num_edges)
    graph.save(args.output, format = "gml")

if __name__ == '__main__':
	main()
